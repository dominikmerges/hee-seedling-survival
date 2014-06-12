
#Read in file
raw <- read.csv('seedlingmaster.csv',header=TRUE,na.strings="")

#Tier 1: Site (not unit), 15 total (3*4+3)
#Tier 2: Plot (4 per  cleracut site, 2 per shelterwood, 12*4+3*2 = 54 total)
#Tier 3: Individual seedlings x time

#Site info

siteid <- 1:15
unit <- c(rep(3,5),rep(6,5),rep(9,5))
#treatments
exclosure <- rep(1:5,3)
opening <- rep(c(1,1,0,0,0),3)
edge <- rep(c(0,0,1,0,0),3)
matrix <- rep(c(0,0,0,1,0),3)
shelter <- rep(c(0,0,0,0,1),3)

site.data <- data.frame(siteid,unit,exclosure,opening,edge,matrix,shelter)

#Plot info

unqplots <- as.character(sort(unique(raw$Plot)))
plotunit <- c(rep(3,18),rep(6,18),rep(9,18))
plot.siteid <- c(c(gl(4,4),5,5),c(gl(4,4),5,5)+5,c(gl(4,4),5,5)+10)
plotid <- 1:54
code <- rep(unqplots,3)
herbivory <- c(rep(c(0,0,1,1),4),0,1)
competition <- c(rep(c(0,1,0,1),4),1,1)
distance <- NA

plot.data <- data.frame(unit,siteid=plot.siteid,plotid,code,herbivory,competition,distance)

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

seedling.data <- data.frame(siteid=seed.siteid,plotid=seed.plotid,pos=raw$Pos,species,age=raw$Age)

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

notes <- raw[,which(ind=='Notes')]
notes <- cbind(rep(NA,nrow(notes)),notes)
names(notes) <- c('Notes',paste('Notes.',1:(ncol(surv)-1),sep=""))
notes <- as.matrix(notes)

#Identify resprouts

sprout.keywords <- c('sprout','resprout','sprout?','sprout/alive','weakly sprouting',
                     'sprout; black?','sprouting','sprout?; dug')

sprout <- matrix(0,ncol=ncol(notes),nrow=nrow(notes))
sprout[which(notes%in%sprout.keywords,arr.ind=TRUE)] <- 1

which(rowSums(sprout)>0)

#Identify problematic seedlings
t3 <- which(reappear==1)
t3 <- t3[!which(reappear==1)%in%which(rowSums(sprout)>0)]
surv[t3,] # should be empty

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
