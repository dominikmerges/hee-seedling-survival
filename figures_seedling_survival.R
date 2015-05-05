source('format_data.R')

#Initial formatting on raw data
seedling <- format.seedling('data/seedlingmaster.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)

#Response variable
surv <- seedling$surv.sprout[keep,]

seedling.covs <- seedling$seedling.data[keep,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species

#Format plot-level variables
nplots <- 54
aspect <- seedling$plot.data$aspect
plot.sitecode <- seedling$plot.data$siteid
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))


#Survival Figure Species*Plant Age
bo = surv[species==0,2:8]
wo = surv[species==1,2:8]

bo.surv = bo.se = vector(length=8)
bo.surv[1] <- 1
for (i in 2:8){
  bo.surv[i] = sum(bo[,(i-1)],na.rm=TRUE) / dim(bo)[1]
  bo.se[i] = sqrt((bo.surv[i]*(1-bo.surv[i]))/length(na.omit(bo[,(i-1)])))
}

wo.surv = wo.se = vector(length=8)
wo.surv[1] <- 1
for (i in 2:8){
  wo.surv[i] = sum(wo[,(i-1)],na.rm=TRUE) / dim(wo)[1]
  wo.se[i] = sqrt((wo.surv[i]*(1-wo.surv[i]))/length(na.omit(wo[,(i-1)])))
}

#bo0 = surv[species==0&age==0,2:8]
#wo0 = surv[species==1&age==0,2:8]

#bo.surv0 = vector(length=8)
#bo.surv0[1] <- 1
#for (i in 2:8){
#  bo.surv0[i] = sum(bo0[,(i-1)],na.rm=TRUE) / dim(bo0)[1]
#}

#wo.surv0 = vector(length=8)
#wo.surv0[1] <- 1
#for (i in 2:8){
#  wo.surv0[i] = sum(wo0[,(i-1)],na.rm=TRUE) / dim(wo0)[1]
#}

plot(bo.surv,type="o",lwd=3,ylim=c(0,1),xaxt='n',xlab="Time",ylab="Proportion Surviving")
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14'))
lines(wo.surv,type="o",lwd=3,col='gray')

for(i in 2:8){
  segments(x0=i,y0=(bo.surv[i]-bo.se[i]),x1=i,y1=(bo.surv[i]+bo.se[i]),col="black",lwd=2)
  segments(x0=i,y0=(wo.surv[i]-wo.se[i]),x1=i,y1=(wo.surv[i]+wo.se[i]),col="gray",lwd=2)
}

#lines(bo.surv0,type="l",lwd=3,lty=2)
#lines(wo.surv0,type="l",lwd=3,lty=2,col='gray')
legend(5,0.8,lwd=3,lty=c(1,1),col=c('gray','black'),
       legend=c('White Oak','Black Oak'))

#########################################################

#Survival by treatment and species


bo.cl = surv[species==0&harvest[seed.plotcode]==1,2:8]
bo.eg = surv[species==0&edge[seed.plotcode]==1,2:8]
bo.sh = surv[species==0&shelter[seed.plotcode]==1,2:8]
bo.ma = surv[species==0&shelter[seed.plotcode]==0&edge[seed.plotcode]==0&harvest[seed.plotcode]==0,2:8]

wo.cl = surv[species==1&harvest[seed.plotcode]==1,2:8]
wo.eg = surv[species==1&edge[seed.plotcode]==1,2:8]
wo.sh = surv[species==1&shelter[seed.plotcode]==1,2:8]
wo.ma = surv[species==1&shelter[seed.plotcode]==0&edge[seed.plotcode]==0&harvest[seed.plotcode]==0,2:8]

bo.surv.cl = bo.surv.eg = bo.surv.sh = bo.surv.ma = c(1,rep(NA,7))
bo.se.cl = bo.se.eg = bo.se.sh = bo.se.ma = vector(length=8)
for (i in 2:8){
  bo.surv.cl[i] = sum(bo.cl[,(i-1)],na.rm=TRUE) / dim(bo.cl)[1]
  bo.surv.eg[i] = sum(bo.eg[,(i-1)],na.rm=TRUE) / dim(bo.eg)[1]
  bo.surv.sh[i] = sum(bo.sh[,(i-1)],na.rm=TRUE) / dim(bo.sh)[1]
  bo.surv.ma[i] = sum(bo.ma[,(i-1)],na.rm=TRUE) / dim(bo.ma)[1]
  bo.se.cl[i] = sqrt((bo.surv.cl[i]*(1-bo.surv.cl[i]))/length(na.omit(bo.cl[,(i-1)])))
  bo.se.eg[i] = sqrt((bo.surv.eg[i]*(1-bo.surv.eg[i]))/length(na.omit(bo.eg[,(i-1)])))
  bo.se.sh[i] = sqrt((bo.surv.sh[i]*(1-bo.surv.sh[i]))/length(na.omit(bo.sh[,(i-1)])))
  bo.se.ma[i] = sqrt((bo.surv.ma[i]*(1-bo.surv.ma[i]))/length(na.omit(bo.ma[,(i-1)])))
}

wo.surv.cl = wo.surv.eg = wo.surv.sh = wo.surv.ma = c(1,rep(NA,7))
wo.se.cl = wo.se.eg = wo.se.sh = wo.se.ma = vector(length=8)
for (i in 2:8){
  wo.surv.cl[i] = sum(wo.cl[,(i-1)],na.rm=TRUE) / dim(wo.cl)[1]
  wo.surv.eg[i] = sum(wo.eg[,(i-1)],na.rm=TRUE) / dim(wo.eg)[1]
  wo.surv.sh[i] = sum(wo.sh[,(i-1)],na.rm=TRUE) / dim(wo.sh)[1]
  wo.surv.ma[i] = sum(wo.ma[,(i-1)],na.rm=TRUE) / dim(wo.ma)[1]
  wo.se.cl[i] = sqrt((wo.surv.cl[i]*(1-wo.surv.cl[i]))/length(na.omit(wo.cl[,(i-1)])))
  wo.se.eg[i] = sqrt((wo.surv.eg[i]*(1-wo.surv.eg[i]))/length(na.omit(wo.eg[,(i-1)])))
  wo.se.sh[i] = sqrt((wo.surv.sh[i]*(1-wo.surv.sh[i]))/length(na.omit(wo.sh[,(i-1)])))
  wo.se.ma[i] = sqrt((wo.surv.ma[i]*(1-wo.surv.ma[i]))/length(na.omit(wo.ma[,(i-1)])))
}

par.default <- par()

par(mfrow=c(1,2),
    oma = c(5,4,0,0) + 0.1,
    mar = c(1,0,1,1) + 0.1)

cols <- c(rgb(red=244,green=125,blue=66, maxColorValue=255),
          rgb(red=241,green=194,blue=50, maxColorValue=255),
          rgb(red=141,green=213,blue=18, maxColorValue=255),
          rgb(red=75,green=142,blue=26, maxColorValue=255))

plot(bo.surv.cl,type="o",lwd=3,ylim=c(0,1),xaxt='n',xlab="Time",ylab="Proportion Surviving",xlim=c(1,8.5),
     col=cols[1],main="Black Oak",pch=20,cex=1.5)
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14'))
lines(bo.surv.eg,type="o",lwd=3,col=cols[2],pch=20,cex=1.5)
lines(bo.surv.sh,type="o",lwd=3,col=cols[3],pch=20,cex=1.5)
lines(bo.surv.ma,type="o",lwd=3,col=cols[4],pch=20,cex=1.5)

text(8.55,0.38,"A")
segments(x0=8.3,y0=0.32,x1=8.3,y1=0.44,lwd=2)
segments(x0=8.3,y0=0.44,x1=8.2,y1=0.44,lwd=2)
segments(x0=8.3,y0=0.32,x1=8.2,y1=0.32,lwd=2)
text(8.55,0.18,"B")

for(i in 2:8){
  segments(x0=i,y0=(bo.surv.cl[i]-bo.se.cl[i]),x1=i,y1=(bo.surv.cl[i]+bo.se.cl[i]),col=cols[1],lwd=2)  
  segments(x0=i-0.15,y0=(bo.surv.cl[i]-bo.se.cl[i]),x1=i+0.15,y1=(bo.surv.cl[i]-bo.se.cl[i]),col=cols[1],lwd=1)
  segments(x0=i-0.15,y0=(bo.surv.cl[i]+bo.se.cl[i]),x1=i+0.15,y1=(bo.surv.cl[i]+bo.se.cl[i]),col=cols[1],lwd=1)
  
  segments(x0=i,y0=(bo.surv.eg[i]-bo.se.eg[i]),x1=i,y1=(bo.surv.eg[i]+bo.se.eg[i]),col=cols[2],lwd=2)
  segments(x0=i-0.15,y0=(bo.surv.eg[i]-bo.se.eg[i]),x1=i+0.15,y1=(bo.surv.eg[i]-bo.se.eg[i]),col=cols[2],lwd=1)
  segments(x0=i-0.15,y0=(bo.surv.eg[i]+bo.se.eg[i]),x1=i+0.15,y1=(bo.surv.eg[i]+bo.se.eg[i]),col=cols[2],lwd=1)
  
  segments(x0=i,y0=(bo.surv.sh[i]-bo.se.sh[i]),x1=i,y1=(bo.surv.sh[i]+bo.se.sh[i]),col=cols[3],lwd=2)
  segments(x0=i-0.15,y0=(bo.surv.sh[i]-bo.se.sh[i]),x1=i+0.15,y1=(bo.surv.sh[i]-bo.se.sh[i]),col=cols[3],lwd=1)
  segments(x0=i-0.15,y0=(bo.surv.sh[i]+bo.se.sh[i]),x1=i+0.15,y1=(bo.surv.sh[i]+bo.se.sh[i]),col=cols[3],lwd=1)
  
  segments(x0=i,y0=(bo.surv.ma[i]-bo.se.ma[i]),x1=i,y1=(bo.surv.ma[i]+bo.se.ma[i]),col=cols[4],lwd=2)
  segments(x0=i-0.15,y0=(bo.surv.ma[i]-bo.se.ma[i]),x1=i+0.15,y1=(bo.surv.ma[i]-bo.se.ma[i]),col=cols[4],lwd=1)
  segments(x0=i-0.15,y0=(bo.surv.ma[i]+bo.se.ma[i]),x1=i+0.15,y1=(bo.surv.ma[i]+bo.se.ma[i]),col=cols[4],lwd=1)
  
}

legend(5,1,lwd=3,col=c(cols[3],cols[4],cols[2],cols[1]),pch=20,
       legend=c('Shelterwood','Forest','Edge','Clearcut'))


##################################################################

plot(wo.surv.cl,type="o",lwd=3,ylim=c(0,1),xaxt='n',yaxt='n',xlab="Time",ylab="Proportion Surviving",xlim=c(1,8.5),
     col=cols[1],main="White Oak",pch=20,cex=1.5)
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14'))
lines(wo.surv.eg,type="o",lwd=3,col=cols[2],pch=20,cex=1.5)
lines(wo.surv.sh,type="o",lwd=3,col=cols[3],pch=20,cex=1.5)
lines(wo.surv.ma,type="o",lwd=3,col=cols[4],pch=20,cex=1.5)

text(8.55,0.42,"A")
segments(x0=8.3,y0=0.36,x1=8.3,y1=0.48,lwd=2)
segments(x0=8.3,y0=0.36,x1=8.2,y1=0.36,lwd=2)
segments(x0=8.3,y0=0.48,x1=8.2,y1=0.48,lwd=2)

for(i in 2:8){
  segments(x0=i,y0=(wo.surv.cl[i]-wo.se.cl[i]),x1=i,y1=(wo.surv.cl[i]+wo.se.cl[i]),col=cols[1],lwd=2)  
  segments(x0=i-0.15,y0=(wo.surv.cl[i]-wo.se.cl[i]),x1=i+0.15,y1=(wo.surv.cl[i]-wo.se.cl[i]),col=cols[1],lwd=1)
  segments(x0=i-0.15,y0=(wo.surv.cl[i]+wo.se.cl[i]),x1=i+0.15,y1=(wo.surv.cl[i]+wo.se.cl[i]),col=cols[1],lwd=1)
  
  segments(x0=i,y0=(wo.surv.eg[i]-wo.se.eg[i]),x1=i,y1=(wo.surv.eg[i]+wo.se.eg[i]),col=cols[2],lwd=2)
  segments(x0=i-0.15,y0=(wo.surv.eg[i]-wo.se.eg[i]),x1=i+0.15,y1=(wo.surv.eg[i]-wo.se.eg[i]),col=cols[2],lwd=1)
  segments(x0=i-0.15,y0=(wo.surv.eg[i]+wo.se.eg[i]),x1=i+0.15,y1=(wo.surv.eg[i]+wo.se.eg[i]),col=cols[2],lwd=1)
  
  segments(x0=i,y0=(wo.surv.sh[i]-wo.se.sh[i]),x1=i,y1=(wo.surv.sh[i]+wo.se.sh[i]),col=cols[3],lwd=2)
  segments(x0=i-0.15,y0=(wo.surv.sh[i]-wo.se.sh[i]),x1=i+0.15,y1=(wo.surv.sh[i]-wo.se.sh[i]),col=cols[3],lwd=1)
  segments(x0=i-0.15,y0=(wo.surv.sh[i]+wo.se.sh[i]),x1=i+0.15,y1=(wo.surv.sh[i]+wo.se.sh[i]),col=cols[3],lwd=1)
  
  segments(x0=i,y0=(wo.surv.ma[i]-wo.se.ma[i]),x1=i,y1=(wo.surv.ma[i]+wo.se.ma[i]),col=cols[4],lwd=2)
  segments(x0=i-0.15,y0=(wo.surv.ma[i]-wo.se.ma[i]),x1=i+0.15,y1=(wo.surv.ma[i]-wo.se.ma[i]),col=cols[4],lwd=1)
  segments(x0=i-0.15,y0=(wo.surv.ma[i]+wo.se.ma[i]),x1=i+0.15,y1=(wo.surv.ma[i]+wo.se.ma[i]),col=cols[4],lwd=1)
  
}




title(xlab = "Time",
      ylab = "Proportion Surviving",
      outer = TRUE, line = 2)

par(par.default)

##############################################################################

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
is.sprout <- sprout.raw[keep2,]

#Seedling-level covariates
nseedlings <- dim(growth)[1]
seedling.covs <- seedling$seedling.data[keep,]
seedling.covs <- seedling.covs[keep2,]
seed.sitecode <- seedling.covs$siteid
seed.plotcode <- seedling.covs$plotid
age <- seedling.covs$age
species <- seedling.covs$species

meangr <- rep(NA,length(nsamples))

for (i in 1:length(nsamples)){
  hold <- growth[i,]
  sprout <- is.sprout[i,]
  
  hold <- hold[sprout!=1]
  
  if(length(hold)>=1){
  meangr[i] <- mean(hold,na.rm=TRUE)
  }
  
}

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

#####Multiple Comparisons

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

########################

#par(mfrow=c(2,1),
#    oma = c(5,4,0,0) + 0.1,
#    mar = c(1,0,1,1) + 0.1)

cols <- c(rgb(red=244,green=125,blue=66, maxColorValue=255),
          rgb(red=241,green=194,blue=50, maxColorValue=255),
          rgb(red=141,green=213,blue=18, maxColorValue=255),
          rgb(red=75,green=142,blue=26, maxColorValue=255))

barplot(rev(dat.bo),col=rev(cols),ylim=c(0,7.75),main="Black Oak",
        names=c('Forest','Shelter','Edge','Clear'),xlab="Treatment",
        ylab="Mean Annual Height Growth (cm)")

structure = rev(c(0.7,1.9,3.1,4.3))
t = rev(c('a','ab','a','b'))
for (i in 1:4){
  segments(x0=structure[i],y0=(dat.bo[i]),x1=structure[i],y1=(dat.bo[i]+se.bo[i]),lwd=2)
  segments(x0=structure[i]-0.2,y0=(dat.bo[i]+se.bo[i]),x1=structure[i]+0.2,y1=(dat.bo[i]+se.bo[i]),lwd=2)
  
  text(x=structure[i],y=(dat.bo[i]+se.bo[i]+0.4),labels=t[i])
}

barplot(rev(dat.wo),col=rev(cols),ylim=c(0,7.75),main="White Oak",
        names=c('Forest','Shelter','Edge','Clear'),xlab="Treatment",
        ylab="Mean Annual Height Growth (cm)")

structure = rev(c(0.7,1.9,3.1,4.3))
t = rev(c('a','ab','a','b'))
for (i in 1:4){
  segments(x0=structure[i],y0=(dat.wo[i]),x1=structure[i],y1=(dat.wo[i]+se.wo[i]),lwd=2)
  segments(x0=structure[i]-0.2,y0=(dat.wo[i]+se.wo[i]),x1=structure[i]+0.2,y1=(dat.wo[i]+se.wo[i]),lwd=2)
  text(x=structure[i],y=(dat.wo[i]+se.wo[i]+0.4),labels=t[i])
}

par(mfrow=c(1,1))