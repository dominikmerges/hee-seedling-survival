#2. Seedling growth (height)
#     Random Covariates:
#     - Site, plot
#     Fixed Covariates:
#     - Canopy cover
#     - Browse at time t-1
#     - Height at time t-1
#     - Competition
#     - Species
#     - Source
#

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('seedlingmaster.csv')

#Only keep seedlings that "established" and were not in shelterwoods
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid<49)

sprout.raw <- seedling$sprout[keep,]

for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    start <- min(which(hold==1),na.rm=TRUE)
    sprout.raw[i,start:dim(sprout.raw)[2]] <- 1
  }
}
#Select only measurement samples
sprout.raw <- sprout.raw[,c(2,4,6)]
end <- numeric(dim(sprout.raw)[1])
for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    end[i] <- min(which(hold==1),na.rm=TRUE) - 1
  } else {end[i]=3}
}

surv <- seedling$surv[keep,c(2,4,6)]
endsurv <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  hold <- surv[i,]
  if(0%in%hold){
    endsurv[i] <- min(which(hold==0),na.rm=TRUE) - 1
  } else {endsurv[i]=3}
}

last <- pmin(end,endsurv)
keep2 <- which(last>0)

growth <- seedling$htgrowth[keep,]
growth <- growth[keep2,]

nsamples <- last[keep2]
last <-last[keep2]


#Seedling-level covariates
nseedlings <- dim(growth)[1]
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
start.height <- seedling.covs$initialhtZ
species <- seedling.covs$species

#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]
browse <- browse[keep2,]
browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],browse[,5]+browse[,6]))
browse[which(browse>1,arr.ind=TRUE)] = 1

#Browse quality control
for (i in 1:nseedlings){
  for (j in 1:last[i]){
    if(is.na(browse[i,j])){
      browse[i,j] <- 0
    }
  }
}

#Format plot-level variables
nplots <- 48
distance <- seedling$plot.data$distanceZ
distance[4] <- 0
aspect <- seedling$plot.data$aspect
canopy <- seedling$plot.data$canopy
canopy[4] <- 0
plot.sitecode <- seedling$plot.data$siteid

#Competition
comp <- seedling$comp.data[,1,][,c(1,3,5,7)]
herb <- seedling$comp.data[,2,][,c(1,3,5,7)]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0
herb[which(is.na(herb),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 12

cucount = matrix(NA, nseedlings, 4)
index=1
for(i in 1:nseedlings){
  for(j in 1:last[i]){
    cucount[i,j] = index
    index = index+1
  }}

###############################

#Bundle data for JAGS

jags.data <- c('growth','nseedlings','nsamples','nplots','nsites','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               ,'age','start.height','browse','species'
               #plot covariates
               ,'distance','aspect','canopy','comp','herb'
)

################################

#Model file

modFile <- 'models/model_seedling_growth.R'

################################

#Parameters to save

params <- c('grand.sd','plot.sd','ind.sd'
            ,'b.browse','b.herb','b.canopy','b.comp','b.distance','b.aspect'
            ,'b.species','b.age','b.browse','b.height'
            ,'fit','fit.new'
)

################################

#Run analysis

require(jagsUI)

growth.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)
