


#Analyses Ideas

#1. Seedling survival
#     Random Covariates:
#     - Site (n=15)
#     - Plot (n=54)
#     Fixed covariates:
#     - Distance to edge (?), how to deal with shelterwoods?
#     - Species
#     - Source (0-0 or 1-0)
#     - Initial height (z-score separated by source)
#     - Canopy cover done
#     - Competition (stem density?) done
#     - Browse at time t-1 done
#     - Time since establishment done
#
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
seedling <- format.seedling('seedlingmaster.csv')

#Only keep seedlings that "established" and were not in shelterwoods
keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid<49)

#Response variable
surv <- seedling$surv.sprout[keep,]

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
start.height <- seedling.covs$initialhtZ
species <- seedling.covs$species

#Browse - simplify to presence/absence for now
browse <- seedling$browse[keep,]
browse[which(browse>1,arr.ind=TRUE)] = 1

#Browse quality control
for (i in 1:nseedlings){
  for (j in 2:nsamples[i]){
    if(!is.na(surv[i,j])&&is.na(browse[i,j-1])){
      browse[i,j-1] <- 0
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
comp <- seedling$comp.data[,1,]
herb <- seedling$comp.data[,2,]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0
herb[which(is.na(herb),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 12
elapsed.raw <- as.matrix(seedling$elapsed)
elapsed <- (elapsed.raw - mean(elapsed.raw))/sd(elapsed.raw)

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

jags.data <- c('surv','nseedlings','nsamples','nplots','nsites','cucount'
               ,'seed.plotcode','plot.sitecode','seed.sitecode'
               #seedling covariates
               ,'age','start.height','browse','species'
               #plot covariates
               ,'distance','aspect','canopy','comp','herb'
               #site covariates
               ,'elapsed','season'
               )

################################

#Model file

modFile <- 'models/model_seedling_survival.R'

################################

#Parameters to save

params <- c('grand.sd','plot.sd'
            ,'b.browse','b.herb','b.canopy','b.comp','b.distance','b.aspect','b.elapsed'
            ,'b.species','b.age','b.browse','b.height','b.season'
            ,'fit','fit.new'
  )

################################

#Run analysis

require(jagsUI)

surv.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)




