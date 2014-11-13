source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established" and died back during study
#Also establish when a seedling first sprouted (if any)

include <- first.sprout <- vector(length=dim(seedling$surv)[1])
sprouted <- rep(0,length(include))

for (i in 1:dim(seedling$surv)[1]){
  if(sum(seedling$sprout[i,],na.rm=TRUE)>0&seedling$surv.sprout[i,1]==1){
    include[i] <- TRUE
    sprouted[i] <- 1
    first.sprout[i] <- which(seedling$sprout[i,]==1)[1]
  } else if(0%in%seedling$surv[i,]&seedling$surv.sprout[i,1]==1){
    include[i] <- TRUE
    first.sprout[i] <- which(seedling$surv[i,]==0)[1]
  } else {include[i] <- FALSE}
}
first.sprout <- first.sprout[include]
sprouted <- sprouted[include]

#Seedling covariates
nseedlings <- length(sprouted)
seedling.covs <- seedling$seedling.data[include,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
start.height <- seedling.covs$initialhtZ
species <- seedling.covs$species

#Format root collar diameter data
rcd.raw <- as.matrix(cbind(seedling$rcd[,1],seedling$rcd[,1],
                           seedling$rcd[,2],seedling$rcd[,2],
                           seedling$rcd[,3],seedling$rcd[,3],
                           seedling$rcd[,4],seedling$rcd[,4]))
rcd.raw <- rcd.raw[include,]
rcd.raw[is.na(rcd.raw[,1])&age==1,1] <- mean(rcd.raw[age==1,1],na.rm=TRUE)
rcd.raw[is.na(rcd.raw[,1])&age==0,1] <- mean(rcd.raw[age==0,1],na.rm=TRUE)

for (i in 1:nseedlings){
  for (j in 2:8){
    if(is.na(rcd.raw[i,j])){
        rcd.raw[i,j] <- rcd.raw[i,(j-1)]  
      }}}

rcd <- vector(length=nseedlings)
for(i in 1:nseedlings){
  rcd[i] <- rcd.raw[i,first.sprout[i]]
}



rcd <- (rcd - mean(rcd,na.rm=TRUE)) / sd(rcd,na.rm=TRUE)

#Browse - simplify to presence/absence for now
browse.raw <- seedling$browse[include,]
browse.raw[which(browse.raw>1,arr.ind=TRUE)] = 1
browse <- vector(length=nseedlings)
for(i in 1:nseedlings){
  browse[i] <- browse.raw[i,(first.sprout[i]-1)]
  if(is.na(browse[i])){browse[i] <- 0}
}

#Format plot-level variables
nplots <- 54
aspect <- seedling$plot.data$aspect
plot.sitecode <- seedling$plot.data$siteid
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

#Competition
comp.raw <- seedling$comp.data[,1,]
comp.raw[which(is.na(comp.raw),arr.ind=TRUE)] <- 0

comp <- vector(length=nseedlings)
for (i in 1:nseedlings){
  comp[i] <- comp.raw[seed.plotcode[i],(first.sprout[i]-1)]
}

comp <- (comp - mean(comp,na.rm=TRUE)) / sd(comp,na.rm=TRUE)

#Site level variables
nsites <- 15

###############################

#Bundle data for JAGS

jags.data <- c('sprouted','nseedlings','nplots','nsites'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               ,'species'
               ,'rcd'
               ,'browse'
               ,'comp'
               #plot covariates
               #,'aspect'
               ,'edge','harvest','shelter'
)

################################

#Model file

modFile <- 'models/model_seedling_sprout.R'

################################

#Parameters to save

params <- c('grand.mean','site.sd','plot.sd'
            ,'b.rcd'
            ,'b.comp'
            ,'b.browse'
            ,'b.species'
            ,'b.edge','b.harvest','b.shelter'
            ,'fit','fit.new'
)

################################

#Run analysis

library(jagsUI)

sprout.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

save(sprout.output,file="output/sprout_output.Rda")

pp.check(sprout.output,'fit','fit.new')



