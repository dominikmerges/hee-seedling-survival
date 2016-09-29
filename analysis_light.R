################################################
##Calculate Available Light Values by Treatment#
################################################

#Read in competition data
comp <- read.csv('data/hee_competition.csv',header=TRUE)[,-c(6:10)]

#Calculate mean canopy cover values by subplot and visit
canMeans <- apply(comp[,6:9],1,mean,na.rm=T)

canopy <- matrix(NA,nrow=max(comp$Plotid),ncol=8)
canopy[,1:2] <- canMeans[1:54]
canopy[,3:4] <- canMeans[55:108]
canopy[,5:6] <- canMeans[109:162]
canopy[,7:8] <- canMeans[163:216]

for (i in 1:54){
  for (j in 3:8){
    if(is.nan(canopy[i,j])){canopy[i,j] <- canopy[i,j-1]}
  }
}

#Calculate available light from canopy cover
light.raw <- 1 - canopy/100

######################################

#For use if light is included as covariate in growth/survival analysis

#Standardize
light <- (light.raw - mean(light.raw,na.rm=T))/sd(light.raw,na.rm=T)
#For growth analysis
light <- light[,c(1,3,5,7)]

######################################

#Create summary table by unit and harvest treatment
light.sum.raw <- data.frame(light=light.raw[,1],unit=seedling$plot.data$unit,code=seedling$plot.data$code)

#Match plot codes to treatments
harvest <- c('1HC','1HN','1NC','1NN','2HC','2HN','2NC','2NN')
edge <- c('3HC','3HN','3NC','3NN')
matrix <- c('4HC','4HN','4NC','4NN')
shelter <- c('HN','NN')

light.sum <- as.data.frame(matrix(NA,nrow=12,ncol=4))
names(light.sum) <- c('unit','treat','light','light.se')
light.sum[,1] <- rep(c(3,6,9),each=4)
light.sum[,2] <- rep(c('matrix','edge','harvest','shelter'),3)

#Assign treatment to light values
for (i in 1:12){
  
  if(light.sum$treat[i]=='matrix'){select=matrix}
  if(light.sum$treat[i]=='edge'){select=edge}
  if(light.sum$treat[i]=='harvest'){select=harvest}
  if(light.sum$treat[i]=='shelter'){select=shelter}
  
  hold <- light.sum.raw$light[light.sum.raw$unit==light.sum$unit[i]&light.sum.raw$code%in%select]
  
  light.sum$light[i] <- round(mean(hold,na.rm=T),2)
  light.sum$light.se[i] <- round(sd(hold,na.rm=T)/sqrt(length(hold)),2)
}

#Nonparametric canopy cover analysis
library(pgirmess)
library(plyr)

treat <- rep('matrix',54)
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
treat[which(edge==1)] <- 'edge'
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
treat[which(harvest==1)] <- 'harvest'
shelter <- c(rep(0,48),rep(1,6))
treat[which(shelter==1)] <- 'shelter'
treat <- as.factor(treat)

canopy.frame <- data.frame(canopy=canopy[,1],treat=treat)

se <- function(x){sd(x,na.rm=T)/length(x)}

ddply(canopy.frame,~treat,summarise,mean=mean(canopy,na.rm=T),sd=se(canopy))

#Test for differences in light between treatments (non-parametric)

canopy.test <- kruskal.test(canopy[,1],treat)

canopy.mc.test <- kruskalmc(canopy[,1],treat)
