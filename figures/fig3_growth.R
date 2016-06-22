################################################
##Oak Seedling Growth Figure (Fig. 3 in paper)##
################################################

#Initial formatting on raw data
source('script_format_data.R')
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
is.sprout <- sprout.raw[keep2,]

meangr <- rep(NA,length(nsamples))
for (i in 1:length(nsamples)){
  hold <- growth[i,]
  sprout <- is.sprout[i,]
  hold <- hold[sprout!=1]
  
  if(length(hold)>=1){
  meangr[i] <- mean(hold,na.rm=TRUE)
  }
}

#Covariates
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

#Summarize by harvest treatment and species

bo.cl = meangr[species==0&harvest[seed.plotcode]==1]
bo.eg = meangr[species==0&edge[seed.plotcode]==1]
bo.sh = meangr[species==0&shelter[seed.plotcode]==1]
bo.ma = meangr[species==0&shelter[seed.plotcode]==0&edge[seed.plotcode]==0&harvest[seed.plotcode]==0]

dat.bo <- c(mean(bo.cl,na.rm=TRUE),mean(bo.eg,na.rm=TRUE),
            mean(bo.sh,na.rm=TRUE),mean(bo.ma,na.rm=TRUE))

se.bo <- c(sd(bo.cl,na.rm=TRUE)/sqrt(length(na.omit(bo.cl))),
           sd(bo.eg,na.rm=TRUE)/sqrt(length(na.omit(bo.eg))),
           sd(bo.sh,na.rm=TRUE)/sqrt(length(na.omit(bo.sh))),
           sd(bo.ma,na.rm=TRUE)/sqrt(length(na.omit(bo.ma))))

wo.cl = meangr[species==1&harvest[seed.plotcode]==1]
wo.eg = meangr[species==1&edge[seed.plotcode]==1]
wo.sh = meangr[species==1&shelter[seed.plotcode]==1]
wo.ma = meangr[species==1&shelter[seed.plotcode]==0&edge[seed.plotcode]==0&harvest[seed.plotcode]==0]

dat.wo <- c(mean(wo.cl,na.rm=TRUE),mean(wo.eg,na.rm=TRUE),
            mean(wo.sh,na.rm=TRUE),mean(wo.ma,na.rm=TRUE))

se.wo <- c(sd(wo.cl,na.rm=TRUE)/sqrt(length(na.omit(wo.cl))),
           sd(wo.eg,na.rm=TRUE)/sqrt(length(na.omit(wo.eg))),
           sd(wo.sh,na.rm=TRUE)/sqrt(length(na.omit(wo.sh))),
           sd(wo.ma,na.rm=TRUE)/sqrt(length(na.omit(wo.ma))))

##############################################

#Test for differences between harvest treatments (for letters)

grbo <- c(na.omit(bo.cl),na.omit(bo.eg),na.omit(bo.sh),na.omit(bo.ma))
trbo <- c(rep('cl',length(na.omit(bo.cl))),rep('eg',length(na.omit(bo.eg))),
          rep('sh',length(na.omit(bo.sh))),rep('ma',length(na.omit(bo.ma))))

grwo <- c(na.omit(wo.cl),na.omit(wo.eg),na.omit(wo.sh),na.omit(wo.ma))
trwo <- c(rep('cl',length(na.omit(wo.cl))),rep('eg',length(na.omit(wo.eg))),
          rep('sh',length(na.omit(wo.sh))),rep('ma',length(na.omit(wo.ma))))

bo <- aov(grbo ~ trbo)
TukeyHSD(bo)

wo <- aov(grwo ~ trwo)
TukeyHSD(wo)

###############################################

#Output figure as TIFF file
tiff(filename="Fig2_Growth.tiff",width=3,height=3.8,units="in",res=300, pointsize=6,
     compression = "lzw",type='cairo')

#Output as PDF (for dissertation)
#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig4-2.pdf",width=5,height=5,family="CM Roman",pointsize=10)

par(mar = c(4,4.5,0.5,2) + 0.1)
par(fig=c(0,1,0.45,1),new=FALSE,mgp=c(2.5,1,0))

cols <- c(rgb(red=244,green=125,blue=66, maxColorValue=255),
          rgb(red=241,green=194,blue=50, maxColorValue=255),
          rgb(red=141,green=213,blue=18, maxColorValue=255),
          rgb(red=75,green=142,blue=26, maxColorValue=255))

cols <- c('white','gray80','gray30','black')

barplot(rev(dat.bo),col=rev(cols),ylim=c(0,7.75), ylab="")
text(1,6.5,'Black Oak',cex=1.3)

structure = rev(c(0.7,1.9,3.1,4.3))
t = rev(c('A','AB','A','B'))
for (i in 1:4){
  segments(x0=structure[i],y0=(dat.bo[i]),x1=structure[i],y1=(dat.bo[i]+se.bo[i]),lwd=1)
  segments(x0=structure[i]-0.2,y0=(dat.bo[i]+se.bo[i]),x1=structure[i]+0.2,y1=(dat.bo[i]+se.bo[i]),lwd=1)
  
  text(x=structure[i],y=(dat.bo[i]+se.bo[i]+0.4),labels=t[i])
}
par(fig=c(0,1,0,0.55),new=TRUE)
barplot(rev(dat.wo),col=rev(cols),ylim=c(0,7.75),
        names=c('Forest','Shelter','Clear Edge','Clear Int'),xlab="Treatment",
        ylab="")
text(1,6.5,'White Oak',cex=1.3)

structure = rev(c(0.7,1.9,3.1,4.3))
t = rev(c('A','AB','A','A'))
for (i in 1:4){
  segments(x0=structure[i],y0=(dat.wo[i]),x1=structure[i],y1=(dat.wo[i]+se.wo[i]),lwd=1)
  segments(x0=structure[i]-0.2,y0=(dat.wo[i]+se.wo[i]),x1=structure[i]+0.2,y1=(dat.wo[i]+se.wo[i]),lwd=1)
  text(x=structure[i],y=(dat.wo[i]+se.wo[i]+0.4),labels=t[i])
}
mtext('Mean Annual Height Growth (cm)',side=2,line=-2,outer=TRUE)

dev.off()
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.18/bin/gswin64c.exe")
#embed_fonts("../dissertation/figures/fig4-2.pdf")
