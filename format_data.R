

format.seedling <- function(eh.file){

#Read in file
raw <- read.csv(eh.file,header=TRUE,na.strings="")

coords <- read.csv('data/plotcoords.csv',header=TRUE)

comp <- read.csv('data/competition.csv',header=TRUE)

#Tier 1: Site (not unit), 15 total (3*4+3)
#Tier 2: Plot (4 per  clearcut site, 2 per shelterwood, 12*4+3*2 = 54 total)
#Tier 3: Individual seedlings x time

#Site info

siteid <- 1:15
unit <- c(rep(3,4),rep(6,4),rep(9,4),3,6,9)
#treatments
exclosure <- c(rep(1:4,3),5,5,5)
opening <- c(rep(c(1,1,0,0),3),0,0,0)
edge <- c(rep(c(0,0,1,0),3),0,0,0)
matrix <- c(rep(c(0,0,0,1),3),0,0,0)
shelter <- c(rep(0,12),1,1,1)

site.data <- data.frame(siteid,unit,exclosure,opening,edge,matrix,shelter)

#Plot info

unqplots <- as.character(sort(unique(raw$Plot)))
plotunit <- c(rep(3,16),rep(6,16),rep(9,16),3,3,6,6,9,9)
plot.siteid <- c(c(gl(4,4)),c(gl(4,4))+4,c(gl(4,4))+8,13,13,14,14,15,15)
plotid <- 1:54
code <- c(rep(unqplots[1:16],3),rep(c("HN","NN"),3))
herbivory <- c(rep(c(0,0,1,1),12),rep(c(0,1),3))
competition <- c(rep(c(0,1,0,1),12),rep(c(1,1),3))
#positive = into harvest, negative = into matrix
distanceZ <- as.numeric(scale(coords[,4]))
distance2Z <- as.numeric(scale((coords[,4] - mean(coords[,4],na.rm=TRUE))^2))
#SW = 1 NE = 0
aspect <- as.numeric(coords[,5]=="SW")
canopy <- as.numeric(scale(comp$DensMean[1:54]))

#Generate competition PCA

#convert to percents and standardize
conv <- c(0.5,3,15,37.5,62.5,85,97.5)
comp$herb <- conv[comp$herb+1]
comp$woody <- conv[comp$woody+1]

pc <- prcomp(formula=~herb+woody+stems10.50+stems50.100+stems100,data=comp[,6:10],center=TRUE,scale=TRUE,na.action=na.exclude)

#Broken stick revealed keep first, second borderline - kept because of the contrast
#First is associated with increasing woody cover and stem density; second is associated with increasing herbaceous cover
#pc1 <- c(0,1,1,1,1)
#pc2 <- c(1,0,0,0,0)
#pc1 %*% pc2

#Rotate, calculate scores and multiply by negative 1 for simplicity
v <- varimax(pc$rotation[,1:2])
scores = apply(comp[,6:10],2,scale) %*% (-1*v$loadings)

#Get in array format
comp.data <- array(NA,dim=c(54,2,8))
index1=1
index2=1
for (i in 1:4){
  comp.data[1:54,1:2,index2:(index2+1)] <- scores[index1:(index1+53),1:2]
  index1 = index1 + 54
  index2 = index2 + 2
}

plot.data <- data.frame(unit=plotunit,siteid=plot.siteid,plotid,code,herbivory,competition,distanceZ,distance2Z,aspect,canopy)

#Seedling info

seed.plotid <- vector(length=dim(raw)[1])
compare <- paste(as.character(plot.data$unit),as.character(plot.data$code),sep="")

for (i in 1:dim(raw)[1]){
  hold <- paste(as.character(raw$Unit[i]),as.character(raw$Plot[i]),sep="")  
  seed.plotid[i] <- which(hold==compare)  
}

seed.siteid <- vector(length=dim(raw)[1])
for (i in 1:length(seed.siteid)){
  seed.siteid[i] <- plot.data$siteid[seed.plotid[i]]
}

species <- rep(0,dim(raw)[1])
species[raw$Species=='W'] <- 1

mean0 <- mean(as.double(as.vector(raw$Ht[raw$Age==0])),na.rm=TRUE)
sd0 <- sd(as.double(as.vector(raw$Ht[raw$Age==0])),na.rm=TRUE)
mean1 <- mean(as.double(as.vector(raw$Ht[raw$Age==1])),na.rm=TRUE)
sd1 <- sd(as.double(as.vector(raw$Ht[raw$Age==1])),na.rm=TRUE)

initialhtZ <- numeric(length(species))
for(i in 1:length(species)){
  if(raw$Age[i]==0){
    initialhtZ[i] <- (as.double(as.vector(raw$Ht[i])) - mean0) / sd0
  } else {
    initialhtZ[i] <- (as.double(as.vector(raw$Ht[i])) - mean1) / sd1
  }
}
initialhtZ[which(is.na(initialhtZ),arr.ind=TRUE)] <- 0


seedling.data <- data.frame(siteid=seed.siteid,plotid=seed.plotid,pos=raw$Pos,species,age=raw$Age,initialhtZ=initialhtZ)

#Seedling survival matrix

ind <- as.vector(sapply(names(raw), 
  function(i){
    strsplit(i,split='[.]')[[1]][1]
}))

surv <- raw[,which(ind=='Surv')]
#Fix values in last survival column
surv[which(surv[,ncol(surv)]=='?'),ncol(surv)] <- 0
surv <- matrix(as.numeric(as.matrix(surv)),nrow=nrow(surv),ncol=ncol(surv))

#Fix seedlings without recorded deaths
for (i in 1:nrow(surv)){
  for (j in 2:ncol(surv)){
    if(!is.na(surv[i,(j-1)])&&surv[i,(j-1)]==1&&is.na(surv[i,j])){
      surv[i,j] <- 0
    }
  }  
}

#Identify seedlings that re-appeared

reappear <- rep(0,nrow(surv))
for (i in 1:nrow(surv)){
  
  if(any(is.na(surv[i,]))){
    
    
    st <- min(which(surv[i,]==0))
    
    if(any(!is.na(surv[i,(st+1):ncol(surv)]))){
      reappear[i] <- 1
    }    
  }  
}

reappear <- rep(0,nrow(surv))
for (i in 1:nrow(surv)){
  
  if(any(surv[i,]==0)){
  
  st <- min(which(surv[i,]==0))
  
  if(any(surv[i,st:ncol(surv)]==1,na.rm=TRUE)){
    reappear[i] <- 1}
  }
}

notes <- raw[,which(ind=='Notes')]
notes <- cbind(rep(NA,nrow(notes)),notes)
names(notes) <- c('Notes',paste('Notes.',1:(ncol(surv)-1),sep=""))
notes <- as.matrix(notes)

#Identify resprouts

sprout.keywords <- c('sprout','resprout','sprout?','sprout/alive','weakly sprouting',
                     'sprout; black?','sprouting','sprout?; dug')

sprout <- matrix(0,ncol=ncol(notes),nrow=nrow(notes))
sprout[which(notes%in%sprout.keywords,arr.ind=TRUE)] <- 1

#which(rowSums(sprout)>0)

#Survival including sprouting

surv.sprout <- surv

for (i in 1:nrow(surv)){
  if(sum(sprout[i,])>0 && 0%in%surv[i,]){
    surv.sprout[i,1:(tail(which(surv[i,]==0),n=1)-1)] = 1
    
    if(!is.na(surv[i,ncol(surv)])&&surv[i,ncol(surv)]==1){
      surv.sprout[i,1:ncol(surv)] <- 1
    }
  }

}

#Identify problematic seedlings
#t3 <- which(reappear==1)
#t3 <- t3[!which(reappear==1)%in%which(rowSums(sprout)>0)]
#surv[t3,] # should be empty

#Leaf damage

lfdmg <- raw[,which(ind=='Lfdmg')]
lfdmg <- matrix(as.numeric(as.matrix(lfdmg)),nrow=nrow(lfdmg),ncol=ncol(lfdmg))


#Sample dates
dateraw <- data.frame(raw[,which(ind=='Date')])

sample.dates <- data.frame()
plant.date <- vector(length=length(siteid))

for (i in 1:length(siteid)){
  
  hold <- raw$Plant.date[seedling.data$siteid==i]
  hold <- na.omit(hold)[1]
  plant.date[i] <- strptime(hold, "%m/%d/%Y")$yday+1
  
  for (j in 1:dim(surv)[2]){
    
    hold <- dateraw[seedling.data$siteid==i,j]
    hold <- na.omit(hold)[1]
    hold <- strptime(hold, "%m/%d/%Y")$yday+1
    
    
    sample.dates[i,j] <-  hold
  }
}

#Get elapsed days since establishment
elapsed <- sample.dates
add.days <- c(0,0,365,365,730,730,1095,1095)
for (i in 1:ncol(sample.dates)){
  elapsed[,i] <- sample.dates[,i] + add.days[i] - sample.dates[,1]
}


#Height

heightraw <- data.frame(raw[,which(ind=='Ht')])
height <- matrix(as.numeric(as.matrix(heightraw)),nrow=nrow(heightraw),ncol=ncol(heightraw))

#Height growth

htgrowth <- matrix(NA,nrow=nrow(height),ncol=(ncol(height)-1))
for (i in 1:ncol(htgrowth)){
  htgrowth[,i] <- height[,i+1] - height[,i]
}

#Root collar Diameter

rcdraw <- data.frame(raw[,which(ind=='Rt')])
rcd <- matrix(as.numeric(as.matrix(rcdraw)),nrow=nrow(rcdraw),ncol=ncol(rcdraw))

#Root collar diameter growth

rcdgrowth <- matrix(NA,nrow=nrow(rcd),ncol=(ncol(rcd)-1))
for (i in 1:ncol(rcdgrowth)){
  rcdgrowth[,i] <- rcd[,i+1] - rcd[,i]
}

#Browse damage

browseraw <- data.frame(raw[,which(ind=='Browse')])

browsedeer <- matrix(NA,nrow=nrow(browseraw),ncol=ncol(browseraw))

browsedeer[which(browseraw==0,arr.ind=TRUE)] <- 0
browseother <- browse <- browsedeer

browsedeer[which((browseraw=='1d'|browseraw=='1D'),arr.ind=TRUE)] <- 1
browsedeer[which((browseraw=='2d'|browseraw=='2D'),arr.ind=TRUE)] <- 2
browsedeer[which((browseraw=='3d'|browseraw=='3D'),arr.ind=TRUE)] <- 3

browseother[which((browseraw=='1r'|browseraw=='1R'),arr.ind=TRUE)] <- 1
browseother[which((browseraw=='2r'|browseraw=='2R'),arr.ind=TRUE)] <- 2
browseother[which((browseraw=='3r'|browseraw=='3R'),arr.ind=TRUE)] <- 3

browse[which((browseraw=='1r'|browseraw=='1R'|browseraw=='1d'|browseraw=='1D'),arr.ind=TRUE)] <- 1
browse[which((browseraw=='2r'|browseraw=='2R'|browseraw=='2d'|browseraw=='2D'),arr.ind=TRUE)] <- 2
browse[which((browseraw=='3r'|browseraw=='3R'|browseraw=='3d'|browseraw=='3D'),arr.ind=TRUE)] <- 3

output <- list(site.data=site.data,plot.data=plot.data,seedling.data=seedling.data,
               comp.data=comp.data,
               surv=surv,surv.sprout=surv.sprout,
               reappear=reappear,sprout=sprout,lfdmg=lfdmg,browse=browse,browsedeer=browsedeer,
               browseother=browseother,height=height,htgrowth=htgrowth,rcd=rcd,
               rcdgrowth=rcdgrowth,plant.date=plant.date,sample.dates=sample.dates,elapsed=elapsed,notes=notes)

return(output)
}