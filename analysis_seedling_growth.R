##########################
#Seedling growth analysis#
##########################

source('script_format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/hee_seedling_master.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$species==1)
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

#Seedling-level covariates
nseedlings <- dim(growth)[1]
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
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
    }}}

#Identify seedlings as having resprouted
is.sprout <- sprout.raw[keep2,]

#Format plot-level variables
nsubplots <- 54
aspect <- seedling$plot.data$aspect
plot.sitecode <- seedling$plot.data$siteid
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

##################################################################
#Competition index

#Read in data
input <- read.csv('data/hee_competition.csv',header=TRUE)[,8:10]

stems <- array(data=NA,dim=c(54,3,4))
stems[,,1] <- as.matrix(input[1:54,])
stems[,,2] <- as.matrix(input[55:108,])
stems[,,3] <- as.matrix(input[109:162,])
stems[,,4] <- as.matrix(input[163:216,])

#Determine how many stems were taller than each oak
ht <- seedling$height[,1:4]
ht <- ht[keep,]
ht <- ht[keep2,]

stem.comp.raw <- matrix(NA,nrow=nseedlings,ncol=4)

for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){    
    hold.stems <- stems[seed.plotcode[i],,j]    
    if (ht[i,j] <= 50) {stem.comp.raw[i,j] = sum(hold.stems[1:3])
    } else if (ht[i,j] > 50 & ht[i,j] <= 100) {stem.comp.raw[i,j] = sum(hold.stems[2:3])
    } else {stem.comp.raw[i,j] = hold.stems[3]}        
  }
}

stem.comp <- (stem.comp.raw - mean(stem.comp.raw,na.rm=TRUE)) / sd(stem.comp.raw,na.rm=TRUE)

#Cleanup missing values
index=0
for (i in 1:nseedlings){
  for (j in 1:nsamples[i]){
    if(is.na(stem.comp[i,j])){
      stem.comp[i,j] <- 0
      index=index+1
    }
  }}

########################################################
#Site level variables
nplots <- 15
elapsed.raw <- c(1,2,3,4)
elapsed <- (elapsed.raw - mean(elapsed.raw))/sd(elapsed.raw)

#Index for posterior predictive check
cucount = matrix(NA, nseedlings, 4)
index=1
for(i in 1:nseedlings){
  for(j in 1:nsamples[i]){
    cucount[i,j] = index
    index = index+1
  }}

#Neglog transformation (Whittaker et al. 2005)
neglog <- function(x){
  return (sign(x)*log(abs(x)+1))
}

inv.neglog <- function(x){
  if(is.na(x)){return(NA)}
  if(x <= 0){
    return(1 - exp(-x))
  } else {return(exp(x)-1)}
}

#Apply to raw data
growth <- apply(growth,c(1,2),neglog)

###############################

#Bundle data for JAGS

jags.data <- c('growth','nseedlings','nsamples','nplots','nsubplots','cucount'#,'year'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               ,'browse','is.sprout'
               ,'aspect'
               ,'edge','harvest','shelter'
               ,'elapsed'
               ,'stem.comp'
)

################################

#Model file

modFile <- 'models/model_seedling_growth.R'

################################

#Parameters to save

params <- c('site.sd','plot.sd'
            ,'seed.sd'
            ,'obs.sd','grand.mean'
            ,'b.elapsed'
            ,'b.browse'
            ,'b.comp'
            ,'b.aspect'
            ,'b.edge','b.harvest','b.shelter'
            ,'b.browse'
            ,'b.sprout'
            ,'b.harvest_comp','b.edge_comp','b.shelter_comp'
            ,'fit','fit.new'
)

################################

#Run analysis

library(jagsUI)

growthbo.output <- autojags(data=jags.data,parameters.to.save=params,model.file=modFile,
                        n.chains=3,iter.increment=10000,n.burnin=10000,n.thin=100,parallel=TRUE)

pp.check(growthbo.output,'fit','fit.new')

#############################################################

growthwo.output <- autojags(data=jags.data,parameters.to.save=params,model.file=modFile,
                            n.chains=3,iter.increment=10000,n.burnin=10000,n.thin=100,parallel=TRUE)

pp.check(growthwo.output,'fit','fit.new')

save(growthbo.output,growthwo.output,file="output/growth_output.Rdata")
