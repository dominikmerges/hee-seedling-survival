


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

#Root collar diameter - relate size and survival

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
        }
      }
    }
  }

rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

species <- seedling.covs$species

#Browse - simplify to presence/absence for now
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
    }
  }
}

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
comp <- seedling$comp.data[,1,]
#herb <- seedling$comp.data[,2,]
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
               #,'age','start.height'
               ,'browse','species','is.sprout'
               #plot covariates
               ,'distance','distance2','aspect','canopy','comp','rcd'
               #,'herb'
               #site covariates
               ,'elapsed','season'
               )

################################

#Model file

modFile <- 'models/model_seedling_survival.R'

################################

#Parameters to save

params <- c('grand.sd','plot.sd','grand.mean'
            ,'b.browse'
            #,'b.herb'
            ,'b.canopy','b.comp','b.distance'
            #,'b.distance2'
            ,'b.aspect','b.elapsed'
            ,'b.species','b.rcd'
            #,'b.age','b.height'
            ,'b.browse','b.season','b.sprout'
            ,'fit','fit.new'
  )

################################

#Run analysis

library(jagsUI)

surv.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=1000,n.burnin=500,n.thin=2,parallel=TRUE)

save(surv.output,file='shiny-seedsurv/survoutput.Rda')


