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
#keep <- which(seedling$surv.sprout[,1]==1&seedling$seedling.data$plotid<49)
keep <- which(seedling$surv.sprout[,1]==1)

#Root collar diameter - relate size and survival

#Survival
surv <- seedling$surv.sprout[keep,]
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
  if(0%in%surv[i,]){nsamples[i] <- nsamples[i]-1}
}

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
      }
    }
  }
}
rcd.raw[,8] <- rcd.raw[,7]

rcd <- (rcd.raw - mean(rcd.raw,na.rm=TRUE)) / sd(rcd.raw,na.rm=TRUE)

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
nplots <- 54
distance <- seedling$plot.data$distanceZ
distance[4] <- 0
distance2 <- distance^2

edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

plot.sitecode <- seedling$plot.data$siteid
exclude <- 1 - seedling$plot.data$herbivory

#Competition
comp <- seedling$comp.data[,1,]
herb <- seedling$comp.data[,2,]
comp[which(is.na(comp),arr.ind=TRUE)] <- 0
herb[which(is.na(herb),arr.ind=TRUE)] <- 0

#Site level variables
nsites <- 15

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
#initial,oct11,may12,oct12,may13,oct13,may14,oct14
season <- c(1,1,0,1,0,1,0,1)

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
               #,'age','start.height'
               ,'rcd'
               ,'species'
               #plot covariates
               #,'distance'
               #,'distance2'
               ,'edge'
               ,'harvest'
               ,'shelter'
               #,'comp','herb'
               ,'exclude'
               #site covariates
               ,'pellet'
               ,'season'
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
            #,'b.herb','b.comp'
            #,'b.distance'
            #,'b.distance2'
            ,'b.edge','b.harvest','b.shelter'
            ,'b.exclude'
            ,'b.species'
            ,'b.rcd'
            #,'b.age','b.height'#,'plot.effect'
            ,'b.pellet'
            ,'b.season'
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
                      n.chains=3,n.iter=5000,n.burnin=3000,n.thin=10,parallel=TRUE)

browse.output <- update(browse.output,n.thin=10,n.iter=2000)

save(browse.output,file="output/browse_output.Rda")