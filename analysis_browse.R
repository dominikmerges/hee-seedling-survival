########################################################
######Analysis of browse intensity on oak seedlings#####
######Proportional odds logistic regression        #####
########################################################

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)

#Get survival data as baseline for when browsing could occur
surv <- seedling$surv.sprout[keep,]
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
  if(0%in%surv[i,]){nsamples[i] <- nsamples[i]-1}
}

#Seedling-level covariates
nseedlings <- dim(surv)[1]
seedling.covs <- seedling$seedling.data[keep,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species
age <- seedling.covs$species

#Format root collar diameter data
rcd.raw <- as.matrix(cbind(seedling$rcd[,1],seedling$rcd[,1],seedling$rcd[,2],seedling$rcd[,2],seedling$rcd[,3],
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
rcd.raw[,8] <- rcd.raw[,7]
rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

#Format height data
ht.raw <- as.matrix(cbind(seedling$height[,1],seedling$height[,1],seedling$height[,2],seedling$height[,2],seedling$height[,3],
                           seedling$height[,3],seedling$height[,4],seedling$height[,4]))
ht.raw <- ht.raw[keep,]
ht.raw[is.na(ht.raw[,1])&age==1,1] <- mean(ht.raw[age==1,1],na.rm=TRUE)
ht.raw[is.na(ht.raw[,1])&age==0,1] <- mean(ht.raw[age==0,1],na.rm=TRUE)

for (i in 1:dim(surv)[1]){
  for (j in 2:8){
    if(!is.na(surv[i,j])){
      if(is.na(ht.raw[i,(j-1)])){
        ht.raw[i,(j-1)] <- ht.raw[i,(j-2)]  
      }}}}
ht.raw[,8] <- rcd.raw[,7]
ht <- (ht.raw - mean(ht.raw,na.rm=TRUE)) / sd(ht.raw,na.rm=TRUE)

ht2.raw <- (ht.raw - mean(ht.raw,na.rm=TRUE))^2

ht2 <- (ht2.raw-mean(ht2.raw,na.rm=TRUE))/sd(ht2.raw,na.rm=TRUE)

#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]

#Final response variable (all browse events)
browse <- browse + 1

#Format plot-level variables
nplots <- 54
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))
plot.sitecode <- seedling$plot.data$siteid
exclude <- 1 - seedling$plot.data$herbivory

#Competition
comp <- seedling$comp.data[,1,]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 15

#Pellet Counts
pellet <- as.matrix(read.csv('data/pellet.csv',header=TRUE))
pindex <- c(1,1,2,2,3,3,4,4)
pelmean <- vector(length=nsites)
for (i in 1:nsites){
  pelmean[i] <- mean(pellet[i,],na.rm=TRUE)
}

#Stick in mean values
pellet[,1] <- pelmean

#Year
year <- matrix(c(1,1,0,0,0,0,0,0,
                 0,0,1,1,0,0,0,0,
                 0,0,0,0,1,1,0,0,
                 0,0,0,0,0,0,1,1),byrow=T,nrow=4)

#Cumulative sample count index
cucount = matrix(NA, nseedlings, 8)
index=1
for(i in 1:nseedlings){
  for(j in 1:nsamples[i]){
    cucount[i,j] = index
    index = index+1
  }}

##############################################
#Does not work in model right now

#Season vector
#summer = 1
#initial,oct11,may12,oct12,may13,oct13,may14,oct14
season <- c(1,1,0,1,0,1,0,1)

##############################################

###############################

#Bundle data for JAGS

jags.data <- c('browse','nseedlings','nplots','nsites','nsamples','year'
               ,'seed.plotcode','seed.sitecode','plot.sitecode'
               #seedling covariates
               #,'rcd'
               ,'ht','ht2'
               ,'species'
               #plot covariates
               ,'edge','harvest','shelter'
               ,'comp'
               ,'exclude'
               ,'cucount'
               #site covariates
               #,'pellet'
               #,'season'
               #,'pindex'
)

################################

#Model file

modFile <- 'models/model_browse.R'

################################

#Parameters to save

params <- c('site.sd','plot.sd'
            ,'seed.sd'
            #,'b.y12','b.y13','b.y14'
            #,'diff.13.12','diff.14.13','diff.14.12'
            ,'b.comp'
            ,'b.edge','b.harvest','b.shelter'
            ,'b.exclude'
            ,'b.species'
            #,'b.rcd'
            ,'b.ht','b.ht2'
            ,'fit','fit.new'
            #,'b.pellet'
            #,'b.season'
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
                      n.chains=3,n.iter=1000,n.burnin=500,n.thin=5,parallel=TRUE)

browse.output <- update(browse.output,n.thin=10,n.iter=4000)

save(browse.output,file="output/browse_output.Rda")
