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
sprout.raw <- sprout.raw[,c(2,4,6,8)]

ht <- seedling$htgrowth[keep,]
end <- numeric(dim(ht)[1])
for (i in 1:dim(ht)[1]){
  hold <- ht[i,]
  if(length(which(!is.na(hold)&hold!=0))<4){
    firstNA <- min(which(is.na(hold)),4,na.rm=TRUE)
    first0 <- min(which(hold==0),4,na.rm=TRUE)
    end[i] <- min(firstNA,first0) - 1
  } else {end[i]=4}
}

keep2 <- which(end>0)

growth <- seedling$htgrowth[keep,]
growth <- growth[keep2,]

nsamples <- end[keep2]



#Seedling-level covariates
nseedlings <- dim(growth)[1]
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
#start.height <- seedling.covs$initialhtZ
rcd.raw <- as.matrix(cbind(seedling$rcd[,1],seedling$rcd[,2],seedling$rcd[,3],
                           seedling$rcd[,4]))
rcd.raw <- rcd.raw[keep,]
rcd.raw <- rcd.raw[keep2,]
rcd.raw[is.na(rcd.raw[,1])&age==1,1] <- mean(rcd.raw[age==1,1],na.rm=TRUE)
rcd.raw[is.na(rcd.raw[,1])&age==0,1] <- mean(rcd.raw[age==0,1],na.rm=TRUE)
for (i in 1:dim(growth)[1]){
  for (j in 1:4){
    if(!is.na(growth[i,j])){
      if(is.na(rcd.raw[i,j])){
        rcd.raw[i,j] <- rcd.raw[i,(j-1)]  
      }
    }
  }
}

rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

rcd[47,2] <- -1.32332005


species <- seedling.covs$species

#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]
browse <- browse[keep2,]
browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],
                          browse[,5]+browse[,6],browse[,7]+browse[,8]))
browse[which(browse>1,arr.ind=TRUE)] = 1

#Browse quality control
for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){
    if(is.na(browse[i,j])){
      browse[i,j] <- 0
    }
  }
}

is.sprout <- sprout.raw[keep2,]

#Format plot-level variables
nplots <- 48
distance <- seedling$plot.data$distanceZ
distance[4] <- 0
distance2 <- seedling$plot.data$distance2Z
distance2[4] <- 0
aspect <- seedling$plot.data$aspect
canopy <- seedling$plot.data$canopy
canopy[4] <- 0
plot.sitecode <- seedling$plot.data$siteid

#Competition
comp <- seedling$comp.data[,1,][,c(1,3,5,7)]
#herb <- seedling$comp.data[,2,][,c(1,3,5,7)]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0
#herb[which(is.na(herb),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 12

cucount = matrix(NA, nseedlings, 4)
index=1
for(i in 1:nseedlings){
  for(j in 1:nsamples[i]){
    cucount[i,j] = index
    index = index+1
  }}

###############################

#Bundle data for JAGS

jags.data <- c('growth','nseedlings','nsamples','nplots','nsites','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               #,'age','start.height'
               ,'rcd','browse','species','is.sprout'
               #plot covariates
               ,'distance'
               #,'distance2'
               ,'aspect'
               ,'canopy'
               ,'comp'
               #,'herb'
)

################################

#Model file

modFile <- 'models/model_seedling_growth.R'

################################

#Parameters to save

params <- c('grand.sd','plot.sd','ind.sd','grand.mean'
            ,'b.browse'
            #,'b.herb',
            ,'b.canopy'
            ,'b.comp','b.distance'
            #,'b.distance2'
            ,'b.aspect'
            ,'b.species'
            #,'b.age'
            ,'b.browse'
            #,'b.height'
            ,'b.sprout'
            ,'b.rcd'
            ,'fit','fit.new'
)

################################

#Run analysis

library(jagsUI)

growth.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=4000,n.burnin=3000,n.thin=2,parallel=TRUE)

pp.check(growth.output,'fit','fit.new')

save(growth.output,file="shiny-seedsurv/growthoutput.Rda")
