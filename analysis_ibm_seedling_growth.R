#########################################
######Seedling growth analysis###########
#########################################

source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)
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

is.sprout <- sprout.raw[keep2,]

#Format plot-level variables
nplots <- 54


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
                      n.chains=3,n.iter=15000,n.burnin=10000,n.thin=2,parallel=TRUE)

ibm.growth.output <- update(growth.output,n.iter=15000,n.thin=10)

save(growth.output,file="output/ibm_growth_output.Rda")
