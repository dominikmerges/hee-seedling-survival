
raw <- read.csv('seedlingmaster.csv',header=TRUE,na.strings="")

seedling.info <- data.frame(id=1:nrow(raw),unit=raw$Unit,treat=raw$Treatment,
                            plot=raw$Plot, pos=raw$Pos, species=raw$Species, age=raw$Age)

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




