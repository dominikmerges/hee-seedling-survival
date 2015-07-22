##############################################
##Seedling Survival Analysis##################
##############################################

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)

#Response variable
surv <- seedling$surv.sprout[keep,][,c(1,2,4,6,8)]
for (i in 1:dim(surv)[1]){
  for (j in 2:5){
    if(is.na(surv[i,j])&!is.na(surv[i,j-1])){      
      if(surv[i,j-1]==1){
      surv[i,j-1] = 0
      break
      }
    }
  }
}

#Calculate number of samples for each seedling
nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
}

#Seedling-level covariates
nseedlings <- dim(surv)[1]
seedling.covs <- seedling$seedling.data[keep,]#[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
t <- matrix(c(1,2,3,4),ncol=4,nrow=length(age),byrow=T)
age <- t + age
species <- seedling.covs$species

st.height <- seedling$height[keep,1:4]
for (i in 1:dim(surv)[1]){
  if(is.na(st.height[i,1])){st.height[i,1] = mean(st.height[,1],na.rm=T)}
}
for (i in 1:dim(surv)[1]){
  for (j in 2:5){
    if(!is.na(surv[i,j])){
      if(is.na(st.height[i,(j-1)])){
        st.height[i,(j-1)] <- st.height[i,(j-2)]  
      }}}}

canopy <- seedling$plot.data$canopy2

#Format browse data (presence/absence only)
#and status of seedling as a sprout
browse <- seedling$browse[keep,]#[keep2,]
browse <- as.matrix(cbind(browse[,1]+browse[,2],browse[,3]+browse[,4],
                          browse[,5]+browse[,6],browse[,7]+browse[,8]))
browse[which(browse>1,arr.ind=TRUE)] = 1

#Browse quality control
for (i in 1:nseedlings){
  for (j in 1:4){
    if(is.na(browse[i,j])){
      browse[i,j] <- 0
    }}}

sprout.raw <- seedling$sprout[keep,]#[keep2,]

for (i in 1:dim(sprout.raw)[1]){
  hold <- sprout.raw[i,]
  if(1%in%hold){
    start <- min(which(hold==1),na.rm=TRUE)
    sprout.raw[i,start:dim(sprout.raw)[2]] <- 1
  }
}
is.sprout <- sprout.raw

###############################

#Bundle data for JAGS

jags.data <- c('surv','nseedlings','nsamples'
               ,'seed.plotcode'
               ,'browse','species','is.sprout'
               ,'st.height','canopy','age'
               )

################################

#Model file

modFile <- 'models/model_ibm_seedling_survival.R'

################################

#Parameters to save

params <- c(#'seed.sd'
            'grand.mean'
            ,'b.species','b.sprout'
            #,'b.browse'
            ,'b.shade'
            #,'b.height'
            ,'b.age'
  )

################################

#Run analysis

library(jagsUI)

surv.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                    n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=FALSE)

save(surv.ibm.output,file='output/surv_ibm_output.Rda')

#equation -0.600 + 0.101 * species + 0. 366 * canopy + 0.576 * age

############

#First 2 years only

for (i in 1:length(nsamples)){
  if(nsamples[i]>3){nsamples[i] = 3}
}

surv12.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                        n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=FALSE)

save(surv12.ibm.output,file='output/surv12_ibm_output.Rda')

#equation -0.531 + 0.197 * species + 0.528 * canopy + 0.440*age

#####################

#Years 3-4

keep2 <- which(surv[,3]==1)

surv <- surv[keep2,3:5]
species <- species[keep2]
nseedlings <- length(species)
age <- age[keep2,3:4]
seed.plotcode <- seed.plotcode[keep2]

nsamples <- numeric(dim(surv)[1])
for (i in 1:dim(surv)[1]){
  nsamples[i] <- length(na.omit(surv[i,]))
}

surv34.ibm.output <- jags(data=jags.data,parameters.to.save=params,model.file=modFile,
                          n.chains=3,n.iter=2000,n.burnin=1000,n.thin=2,parallel=FALSE)

save(surv34.ibm.output,file='output/surv34_ibm_output.Rda')

#equation 2.596 - 0.049 * species - 0.733 * canopy + -0.01 * age
