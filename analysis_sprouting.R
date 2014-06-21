source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('seedlingmaster.csv')

#Only keep seedlings that "established" and were not in shelterwoods
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid<49)

surv.raw <- seedling$surv[keep,]
sprout.raw <- seedling$sprout[keep,]

include <- vector(length=dim(surv.raw)[1])

for (i in 1:dim(surv.raw)[1]){
  if(0%in%surv.raw[i,]){
    include[i] <- TRUE
  } else if(sum(sprout.raw[i,],na.rm=TRUE)>0){
    include[i] <- TRUE
  } else {include[i] <- FALSE}
}

sprout.raw2 <- sprout.raw[include,]
sprouted <- numeric(dim(sprout.raw2)[1])
for (i in 1:dim(sprout.raw2)[1]){
  if(sum(sprout.raw2[i,])>0){
    sprouted[i] <- 1
  }
}

#Seedling-level covariates
nseedlings <- length(sprouted)
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[include,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
start.height <- seedling.covs$initialhtZ
species <- seedling.covs$species

#Format plot-level variables
nplots <- 48
distance <- seedling$plot.data$distanceZ
distance[4] <- 0
aspect <- seedling$plot.data$aspect
canopy <- seedling$plot.data$canopy
canopy[4] <- 0
plot.sitecode <- seedling$plot.data$siteid

#Site level variables
nsites <- 12

###############################

#Bundle data for JAGS

jags.data <- c('sprouted','nseedlings','nplots','nsites','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               ,'age','start.height','species'
               #plot covariates
               ,'distance','aspect','canopy'
)

################################

#Model file

modFile <- 'models/model_seedling_sprout.R'

################################

#Parameters to save

params <- c('grand.sd','plot.sd'
            ,'b.canopy','b.distance','b.aspect'
            ,'b.species','b.age','b.height'
            ,'fit','fit.new'
)

################################

#Run analysis

require(jagsUI)

sprout.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                      n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)

pp.check(sprout.output,'fit','fit.new')



