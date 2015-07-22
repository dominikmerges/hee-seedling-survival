#########################################
######Seedling growth analysis###########
#########################################

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$age==1)
sprout.raw <- seedling$sprout[keep,]

#Keep track of when seedlings became sprouts
for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    start <- min(which(hold==1),na.rm=TRUE)
    sprout.raw[i,start:dim(sprout.raw)[2]] <- 1
  }
}
sprout.raw <- sprout.raw[,c(2,4,6,8)]

#Format and clean up height growth data
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
#Only keep seedlings which have at least one recorded growth in height
#(i.e., did not die in period 2)
keep2 <- which(end>0)
growth <- seedling$htgrowth[keep,]
growth <- growth[keep2,]
nsamples <- end[keep2]
age <- seedling$seedling.data$age[keep][keep2]

st.height <- seedling$height[keep,]
st.height <- st.height[keep2,][,1:4]

#Seedling-level covariates
nseedlings <- dim(growth)[1]
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species
canopy <- seedling$plot.data$canopy2



is.sprout <- sprout.raw[keep2,]

keep3 <- which(rowSums(is.sprout)<1&age==1)

growth <- growth[keep3,]

nseedlings <- dim(growth)[1]
#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]
browse <- browse[keep2,]
browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],
                          browse[,5]+browse[,6],browse[,7]+browse[,8]))
browse[which(browse>1,arr.ind=TRUE)] = 1
browse <- browse[keep3,]

#Browse quality control
for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){
    if(is.na(browse[i,j])){
      browse[i,j] <- 0
    }}}

#Seedling-level covariates

seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,][keep3,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species
canopy <- seedling$plot.data$canopy2

#Format plot-level variables
nplots <- 54

##########################

#Experimenting with neglog transformation

neglog <- function(x){
  
  return (sign(x)*log(abs(x)+1))
  
}

inv.neglog <- function(x){
  if(is.na(x)){return(NA)}
  if(x <= 0){
    return(1 - exp(-x))
  } else {return(exp(x)-1)}
}

growth <- apply(growth,c(1,2),neglog)


###############################

#Bundle data for JAGS

jags.data <- c('growth','nseedlings','nsamples'
               ,'seed.plotcode'
               ,'canopy','browse','species','is.sprout','st.height'
)

################################

#Model file

modFile <- 'models/model_ibm_seedling_growth.R'

################################

#Parameters to save

params <- c('seed.sd','obs.sd','grand.mean'
            ,'b.browse'
            ,'b.light'
            ,'b.species'
            ,'b.browse'
            ,'b.sprout'
            ,'b.height'
)

################################

#Run analysis

library(jagsUI)

ibm.growth.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=FALSE)

ibm.growth.output <- update(growth.output,n.iter=15000,n.thin=10)

save(ibm.growth.output,file="output/ibm_growth_output.Rda")

#equation 1.24 + rnorm(1,0,0.248) - 0.94 * browse - 0.488 * canopy + 0.116 * species + rnorm(1,0,1.188)

#################################

#First 2 years only

for (i in 1:length(nsamples)){
  if(nsamples[i]>2){nsamples[i] = 2}
}

library(jagsUI)

growth <- apply(growth,c(1,2),neglog)

ibm.growth12.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                            n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=FALSE)

save(ibm.growth12.output,file="output/ibm_growth12_output.Rda")

simgrowth <- matrix(NA,nrow=nrow(growth),ncol=2)
for (i in 1:dim(growth)[1]){
  ymean <- rnorm(1,0,0.157)
  for (j in 1:nsamples[i]){
    simgrowth[i,j] <- 0.991 + ymean - 0.871*browse[i,j] + -0.238 * canopy[seed.plotcode[i]] + 0.085 * species[i] + rnorm(1,0,1.016)
  }
}

par(mfrow=c(2,1))
hist(growth[,1:2][growth>-10],freq=F,xlim=c(-4,4.5),ylim=c(0,0.50),
     xlab="Yearly Growth (cm)",main="Actual Seedling Growth",col='gray',breaks=15)
hist(simgrowth[,1:2][simgrowth>-10],freq=F,xlim=c(-4,4.5),ylim=c(0,0.50),
     xlab="Yearly Growth (cm)",main="Sim Seedling Growth",col='red',breaks=15,add=F)

act.growth <- apply(growth,c(1,2),inv.neglog)
act.sim <- apply(simgrowth,c(1,2),inv.neglog)

par(mfrow=c(2,1))
hist(act.growth[,1:2][act.growth[,1:2]>-10],freq=F,xlim=c(-10,50),ylim=c(0,0.2),
     xlab="Yearly Growth (cm)",main="Actual Seedling Growth",col='gray',breaks=15)
hist(act.sim[,1:2][act.sim>-10&act.sim<30],freq=F,xlim=c(-10,50),ylim=c(0,0.2),
     xlab="Yearly Growth (cm)",main="Sim Seedling Growth",col='red',breaks=15,add=F)


########################################################################################

#Second 2 years only

keep5 <- which(!is.na(growth[,3]))
growth <- growth[keep5,3:4]
browse <- browse[keep5,3:4]
seed.plotcode <- seed.plotcode[keep5]
species <- species[keep5]
nseedlings <- length(species)
browse[24,2] <- 0
browse[51,1] <- 0


nsamples <- vector(length=length(species))
for(i in 1:length(species)){
  if(is.na(growth[i,2])){
    nsamples[i] <- 1
  } else {nsamples[i] <- 2}
}

growth <- apply(growth,c(1,2),neglog)

ibm.growth34.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                            n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=FALSE)

save(ibm.growth34.output,file="output/ibm_growth34_output.Rda")

simgrowth <- matrix(NA,nrow=nrow(growth),ncol=2)
for (i in 1:dim(growth)[1]){
  ymean <- rnorm(1,0,0.199)
  for (j in 1:nsamples[i]){
    simgrowth[i,j] <- 2.091 + ymean - 0.976*browse[i,j] + -1.733 * canopy[seed.plotcode[i]] + 0.468 * species[i] + rnorm(1,0,1.468)
  }
}

par(mfrow=c(2,1))
hist(growth[,1:2][growth>-10],freq=F,xlim=c(-4,8),ylim=c(0,0.30),
     xlab="Yearly Growth (cm)",main="Actual Seedling Growth",col='gray',breaks=15)
hist(simgrowth[,1:2][simgrowth>-10],freq=F,xlim=c(-4,8),ylim=c(0,0.30),
     xlab="Yearly Growth (cm)",main="Sim Seedling Growth",col='red',breaks=15,add=F)

inv.neglog <- function(x){
  if(is.na(x)){return(NA)}
  if(x <= 0){
    return(1 - exp(-x))
  } else {return(exp(x)-1)}
}

act.growth <- apply(growth,c(1,2),inv.neglog)
act.sim <- apply(simgrowth,c(1,2),inv.neglog)

par(mfrow=c(2,1))
hist(act.growth[,1:2][act.growth>-10],freq=F,xlim=c(-10,150),ylim=c(0,0.10),
     xlab="Yearly Growth (cm)",main="Actual Seedling Growth",col='gray',breaks=15)
hist(act.sim[,1:2][act.sim>-10&act.sim<150],freq=F,xlim=c(-10,150),ylim=c(0,0.10),
     xlab="Yearly Growth (cm)",main="Sim Seedling Growth",col='red',breaks=15,add=F)


