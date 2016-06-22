############################
#Seedling survival analysis#
############################

source('script_format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established" and are of proper species
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$species==1)

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


sprout.time <- array(NA,dim=dim(is.sprout))
for(i in 1:dim(sprout.time)[1]){
  
  if(is.sprout[i,1]==1){sprout.time[i,1]=1}else{sprout.time[i,1]=0}
  
  for (j in 2:dim(sprout.time)[2]){
    sprout.time[i,j] <- sum(is.sprout[i,1:j]) 
  }
}

#Browse quality control
for (i in 1:nseedlings){
  for (j in 2:nsamples[i]){
    if(!is.na(surv[i,j])&&is.na(browse[i,j-1])){
      browse[i,j-1] <- 0
    }}}

#Format plot-level variables
nsubplots <- 54
aspect <- seedling$plot.data$aspect
plot.sitecode <- seedling$plot.data$siteid
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

##################################################################
#Competition

input <- read.csv('data/competition.csv',header=TRUE)[,8:10]

stems <- array(data=NA,dim=c(54,3,4))
stems[,,1] <- as.matrix(input[1:54,])
stems[,,2] <- as.matrix(input[55:108,])
stems[,,3] <- as.matrix(input[109:162,])
stems[,,4] <- as.matrix(input[163:216,])

init <- seedling$seedling.data$initialht[keep]

#Determine how many competitors are taller than a given oak
ht <- seedling$height[,1:4]
ht <- ht[keep,]

stem.comp.raw <- matrix(NA,nrow=nseedlings,ncol=8)
samp.ind <- c(1,1,2,2,3,3,4,4)

for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){    
    hold.stems <- stems[seed.plotcode[i],,samp.ind[j]]
    
    if(is.na(ht[i,samp.ind[j]])){
      if(is.na(init[i])){ht[i,samp.ind[j]] <- 0
      } else {ht[i,samp.ind[j]] <- init[i]}}
    
    
    if (ht[i,samp.ind[j]] <= 50) {stem.comp.raw[i,j] = sum(hold.stems[1:3])
    } else if (ht[i,samp.ind[j]] > 50 & ht[i,samp.ind[j]] <= 100) {stem.comp.raw[i,j] = sum(hold.stems[2:3])
    } else {stem.comp.raw[i,j] = hold.stems[3]}        
  }
}

stem.comp <- (stem.comp.raw - mean(stem.comp.raw,na.rm=TRUE)) / sd(stem.comp.raw,na.rm=TRUE)

#Clean up missing values
index=0
for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){
    if(is.na(stem.comp[i,j])){
      stem.comp[i,j] <- 0
      index=index+1
    }
  }}

#######################################################################

#Site level variables
nplots <- 15
elapsed.raw <- c(1,1,2,2,3,3,4,4)
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

jags.data <- c('surv','nseedlings','nsamples','nplots','nsubplots','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               ,'browse','is.sprout'
               ,'edge','harvest','shelter'
               ,'aspect'
               ,'stem.comp'
               ,'rcd'
               ,'elapsed'
               ,'season'
               )

################################

#Model file

modFile <- 'models/model_seedling_survival.R'

################################

#Parameters to save

params <- c('site.sd','plot.sd'
            ,'seed.sd'
            ,'grand.mean'
            ,'b.browse'
            ,'b.comp'
            ,'b.edge','b.harvest','b.shelter'
            ,'b.aspect'
            ,'b.elapsed'
            ,'b.rcd'
            ,'b.browse'
            ,'b.season'
            ,'b.sprout'
            ,'fit','fit.new'
  )

################################

#Run analysis

library(jagsUI)

survbo.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)

survbo.output <- update(survbo.output,n.iter=1000,n.thin=10)

save(survbo.output,file='output/survbo_output.Rda')


####################################

survwo2.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)

survwo.output <- update(survwo.output,n.iter=2000,n.thin=5)

save(survwo.output,file='output/survwo_output.Rdata')
