##################################
##Plot Characteristics (Table 1)##
##################################

#Forest structure data summary

#Read in full 2013 inventory dataset
invdata <- read.csv('data/hee_inv_data.csv',header=T)

#Remove dead/harvested trees
invdata <- invdata[invdata$osd!='D',]
invdata <- invdata[!invdata$cond%in%c('C7','C8'),]
invdata$ba.dens <- invdata$ba*as.numeric(as.character(invdata$density))

#List overstory plots in harvested areas by unit
harvest3 <- c('U3-L3','U3-M3','U3-N3','U3-F6','U3-E6','U3-D6')
shelter3 <- c('U3-T4','U3-U4','U3-F2','U3-G2','U3-H2')
harvest6 <- c('U6-N7','U6-M6','U6-J3','U6-I2')
shelter6 <- c('U6-C6','U6-D6','U6-E7','U6-I6','U6-H7','U6-I7')
harvest9 <- c('U9-B6','U9-C6','U9-D6','U9-E6','U9-L5','U9-M5','U9-N5','U9-O5','U9-N5','U9-P5')
shelter9 <- c('U9-B3','U9-A4','U9-B4','U9-M3','U9-L3')

#Generate summary table of structure metrics by plot
all <- unique(invdata$unitplot)
unit <- as.numeric(substr(all,2,2))
all <- all[unit%in%c(3,6,9)]

harv <- c(harvest3,harvest6,harvest9)
shelt <- c(shelter3,shelter6,shelter9)

summary <- data.frame(plot=all)

n.matrix <- n.harvest <- n.shelter <- rep(0,3)

for (i in 1:length(all)){
  
  unit <- as.numeric(substr(all[i],2,2))
  if(!unit%in%c(3,6,9)){next}
  
  summary$unit[i] <- unit
  
  if(all[i]%in%harv){
    summary$treat[i] <- 'harvest'
    n.harvest[unit/3] <- n.harvest[unit/3]+1
  } else if(all[i]%in%shelt){
    summary$treat[i] <- 'shelter'
    n.shelter[unit/3] <- n.shelter[unit/3]+1
  } else {
      summary$treat[i] <- 'matrix'
      n.matrix[unit/3] <- n.matrix[unit/3]+1
      }
  
  hold <- invdata[invdata$unitplot==all[i],]
  hold$dbh[which(is.na(hold$dbh))] <- 0
  
  summary$stem.dens[i] <- sum(as.numeric(as.character(hold$density)),na.rm=T)
  summary$ba[i] <- sum(hold$ba.dens,na.rm=T)
  summary$ov.stem.dens[i] <- sum(as.numeric(as.character(hold$density[hold$dbh>=30])))
  summary$ov.ba[i] <- sum(hold$ba.dens[hold$dbh>=30],na.rm=T)
}

#Sample sizes
N <- cbind(n.matrix,n.harvest,n.shelter)

#Generate final summary table by unit and harvest treatment
final.summary <- as.data.frame(matrix(NA,nrow=9,ncol=10))
names(final.summary)[1:2] <- c('unit','treat')
names(final.summary)[3:10] <- c('stem.dens','stem.dens.se','ba','ba.se','ov.stem.dens',
                                'ov.stem.dens.se','ov.ba','ov.ba.se')
final.summary$treat <- rep(c('matrix','harvest','shelter'),3)
final.summary$unit <- rep(c(3,6,9),each=3)

treats <- c('matrix','harvest','shelter')
units <- c(3,6,9)

index <- 1
for (i in 1:3){
  
  for (j in 1:3){
    
    hold <- summary[summary$unit==units[i]&summary$treat==treats[j],]
    final.summary$stem.dens[index] <- round(mean(hold$stem.dens,na.rm=T),2)
    final.summary$stem.dens.se[index] <- round(sd(hold$stem.dens,na.rm=T)/sqrt(N[i,j]),2)
    
    final.summary$ba[index] <- round(mean(hold$ba,na.rm=T),2)
    final.summary$ba.se[index] <- round(sd(hold$ba,na.rm=T)/sqrt(N[i,j]),2)
    
    final.summary$ov.stem.dens[index] <- round(mean(hold$ov.stem.dens,na.rm=T),2)
    final.summary$ov.stem.dens.se[index] <- round(sd(hold$ov.stem.dens,na.rm=T)/sqrt(N[i,j]),2)
    
    final.summary$ov.ba[index] <- round(mean(hold$ov.ba,na.rm=T),2)
    final.summary$ov.ba.se[index] <- round(sd(hold$ov.ba,na.rm=T)/sqrt(N[i,j]),2)
    index <- index + 1
  }
  
}

###############################################

#Available light summary

#Read in competition data
comp <- read.csv('data/competition.csv',header=TRUE)[,-c(6:10)]

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

#Summary for inclusion in table

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

for (i in 1:12){
  
  if(light.sum$treat[i]=='matrix'){select=matrix}
  if(light.sum$treat[i]=='edge'){select=edge}
  if(light.sum$treat[i]=='harvest'){select=harvest}
  if(light.sum$treat[i]=='shelter'){select=shelter}
  
  hold <- light.sum.raw$light[light.sum.raw$unit==light.sum$unit[i]&light.sum.raw$code%in%select]
  
  light.sum$light[i] <- round(mean(hold,na.rm=T),2)
  light.sum$light.se[i] <- round(sd(hold,na.rm=T)/sqrt(length(hold)),2)
}



