##############################################
##Seedling Survival Analysis##################
##############################################

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)

#Response variable
surv <- seedling$surv.sprout[keep,]

#Calculate number of samples for each seedling
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
}

#Seedling-level covariates
nseedlings <- dim(surv)[1]
seedling.covs <- seedling$seedling.data[keep,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
species <- seedling.covs$species

#Root collar diameter format (stand-in for age and initial size)
rcd.raw <- as.matrix(cbind(seedling$rcd[,1],seedling$rcd[,2],seedling$rcd[,2],seedling$rcd[,3],
                 seedling$rcd[,3],seedling$rcd[,4],seedling$rcd[,4]))
rcd.raw <- rcd.raw[keep,]
rcd.raw[is.na(rcd.raw[,1])&age==1,1] <- mean(rcd.raw[age==1,1],na.rm=TRUE)
rcd.raw[is.na(rcd.raw[,1])&age==0,1] <- mean(rcd.raw[age==0,1],na.rm=TRUE)

for (i in 1:dim(surv)[1]){
  for (j in 2:8){
    if(!is.na(surv[i,j])){
      if(is.na(rcd.raw[i,(j-1)])){
        rcd.raw[i,(j-1)] <- rcd.raw[i,(j-2)]  
        }}}}

rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

#Format browse data (presence/absence only)
#and status of seedling as a sprout
browse <- seedling$browse[keep,]
browse[which(browse>1,arr.ind=TRUE)] = 1

sprout.raw <- seedling$sprout[keep,]

for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    start <- min(which(hold==1),na.rm=TRUE)
    sprout.raw[i,start:dim(sprout.raw)[2]] <- 1
  }
}
is.sprout <- sprout.raw

#Browse quality control
for (i in 1:nseedlings){
  for (j in 2:nsamples[i]){
    if(!is.na(surv[i,j])&&is.na(browse[i,j-1])){
      browse[i,j-1] <- 0
    }}}

#Format plot-level variables
nplots <- 54
aspect <- seedling$plot.data$aspect
plot.sitecode <- seedling$plot.data$siteid
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

#Competition
comp <- seedling$comp.data[,1,]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 15
elapsed.raw <- as.matrix(seedling$elapsed)
elapsed <- (elapsed.raw - mean(elapsed.raw))/sd(elapsed.raw)

#Season vector
#summer = 1
#initial,oct11,may12,oct12,may13,oct13,may14,oct14
season <- c(1,1,0,1,0,1,0,1)

#Year
year <- matrix(c(1,1,0,0,0,0,0,0,
                 0,0,1,1,0,0,0,0,
                 0,0,0,0,1,1,0,0,
                 0,0,0,0,0,0,1,1),byrow=T,nrow=4)

#Sample index for posterior predictive check
cucount = matrix(NA, nseedlings, 8)
index=1
for(i in 1:nseedlings){
  for(j in 2:nsamples[i]){
    cucount[i,j] = index
    index = index+1
  }}

###############################

#Bundle data for JAGS

jags.data <- c('surv','nseedlings','nsamples','nplots','nsites','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               ,'browse','species','is.sprout'
               #plot covariates
               ,'edge','harvest','shelter'
               ,'aspect','comp','rcd'
               #site covariates
               ,'elapsed','season'
               )

################################

#Model file

modFile <- 'models/model_seedling_survival.R'

################################

#Parameters to save

params <- c('site.sd','plot.sd','seed.sd','grand.mean'
            ,'b.browse'
            ,'b.comp'
            ,'b.edge','b.harvest','b.shelter'
            ,'b.aspect'
            #,'b.elapsed'
            ,'b.species','b.rcd'
            ,'b.browse','b.season','b.sprout'
            ,'fit','fit.new'
  )

################################

#Run analysis

library(jagsUI)

surv.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=TRUE)

surv.output <- update(surv.output,n.iter=5000,n.thin=10)

save(surv.output,file='output/surv_output.Rda')


