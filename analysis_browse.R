#3. Browse intensity (how to calculate?)
#     Random Covariates:
#     - Site, plot
#     Fixed Covariates:
#     - Distance to edge
#     - Treatment?
#     - Plant density
#     - Use (pellet counts)
#     - Age of opening

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established" and were not in shelterwoods
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid<49)

#Survival
surv <- seedling$surv.sprout[keep,]
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
}

nsamples <- nsamples

#Seedling-level covariates
nseedlings <- dim(surv)[1]
seedling.covs <- seedling$seedling.data[keep,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
start.height <- seedling.covs$initialhtZ
species <- seedling.covs$species

#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]

#Browse quality control
for (i in 1:nseedlings){
  for (j in 2:nsamples[i]){
    if(!is.na(surv[i,j])&&is.na(browse[i,j-1])){
      browse[i,j-1] <- 0
    }
  }
}

#Final response variable (all browse events)
browse <- browse + 1

#Format plot-level variables
nplots <- 48
distance <- seedling$plot.data$distanceZ
distance[4] <- 0
distance2 <- distance^2

plot.sitecode <- seedling$plot.data$siteid
exclude <- 1 - seedling$plot.data$herbivory

#Competition
comp <- seedling$comp.data[,1,]
herb <- seedling$comp.data[,2,]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0
herb[which(is.na(herb),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 12

#Pellet Counts
pellet <- as.matrix(read.csv('data/pellet.csv',header=TRUE))
#pellet <- as.matrix(pellet.raw)*15
pindex <- c(1,1,2,2,3,3,4,4)
pelmean <- vector(length=nsites)

for (i in 1:nsites){
  #site.dist[i] <- mean(plot.data$distanceZ[plot.data$siteid==i],na.rm=TRUE)
  pelmean[i] <- mean(pellet[i,],na.rm=TRUE)
}

#pelmean <- as.numeric(scale(pelmean))

#Stick in mean values
pellet[,1] <- pelmean

#Season vector
#summer = 1
season <- c(1,0,1,0,1,0,1,0)

#Index

cucount = matrix(NA, nseedlings, 8)
index=1
for(i in 1:nseedlings){
  for(j in 2:nsamples[i]){
    cucount[i,j] = index
    index = index+1
  }}

###############################

#Bundle data for JAGS

jags.data <- c('browse','nseedlings','nplots','nsites','nsamples'
               ,'seed.plotcode','seed.sitecode'
               #seedling covariates
               ,'age','start.height','species'
               #plot covariates
               ,'distance'
               ,'comp','herb','exclude'
               #site covariates
               ,'pellet'
               #,'season'
               #,'pelmean','site.dist'
               ,'pindex'
               #,'cucount'
)

################################

#Model file

modFile <- 'models/model_browse.R'

################################

#Parameters to save

params <- c('site.sd','plot.sd'
            ,'b.herb','b.comp','b.distance'
            ,'b.exclude'
            ,'b.species','b.age','b.height'#,'plot.effect'
            ,'b.pellet'
            #,'b.season'
            #,'fit','fit.new'
            #,'site.effect'
)

################################

inits <- function(){
  list(
    "tau0" = matrix(c(-0.5,0,0.5),nrow=3,ncol=8)
    )
}

#Run analysis

require(jagsUI)

browse.output <- jags(data=jags.data,inits=inits,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)

browse.output <- update(browse.output,parameters.to.save=params, n.iter=2)

pp.check(browse.output,'fit','fit.new')