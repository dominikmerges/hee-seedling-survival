##################################################
##Oak Seedling Survival Figure (Fig. 2 in paper)##
##################################################

#Initial formatting on raw data
source('function_format_data.R')
seedling <- format.seedling('data/hee_seedling_master.csv')

#Only keep seedlings that "established"
keep <- which(seedling$surv.sprout[,1]==1)

#Response variable
surv <- seedling$surv.sprout[keep,]

#Covariates
seedling.covs <- seedling$seedling.data[keep,]
seed.plotcode <- seedling.covs$plotid
species <- seedling.covs$species
edge <- c(rep(c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0),3),rep(0,6))
harvest <- c(rep(c(1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0),3),rep(0,6))
shelter <- c(rep(0,48),rep(1,6))

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
########################################
##Paper figure code

#library(extrafont)
#font_install('fontcm')
#loadfonts()
#pdf(file="../dissertation/figures/fig4-1.pdf",width=5,height=5,family="CM Roman",pointsize=10)

##############################################
#For paper
#Output figure as TIFF file
tiff(filename="Fig1_Surv.tiff",width=3,height=3.8,units="in",res=300, pointsize=6,
     compression = "lzw",type='cairo')

par(mfrow=c(2,1),
    oma = c(4,0,0,0) + 0.1,
    mar = c(0,4,1,1) + 0.1)

cols <- c('gray','gray','black','black')

ltys = c(1,2,1,2)

lwds = 1

#########################################
#For presentations
png(filename="figures/seedling_survival.png",width=6,height=3.8,units="in",res=400, pointsize=8,
    type='cairo')

par(mfrow=c(1,2),
    oma = c(4,0,0,0) + 0.1,
    mar = c(0,4,1,0) + 0.1)

#For presentations
cols <- c(rgb(red=244,green=125,blue=66, maxColorValue=255),
          rgb(red=241,green=194,blue=50, maxColorValue=255),
          rgb(red=141,green=213,blue=18, maxColorValue=255),
          rgb(red=75,green=142,blue=26, maxColorValue=255))

ltys = rep(1,4)

lwds = 2

#########################################

plot(bo.surv.cl,type="o",lwd=lwds,ylim=c(0,1),xaxt='n',xlab="Time",ylab="",xlim=c(1,8.5),
     col=cols[1],pch=20,cex=1.5)
text(2,0.1,'Black Oak',cex=1.3)
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14')) #For presentations

text(8.55,0.38,"A")
segments(x0=8.3,y0=0.32,x1=8.3,y1=0.44,lwd=1)
segments(x0=8.3,y0=0.44,x1=8.2,y1=0.44,lwd=1)
segments(x0=8.3,y0=0.32,x1=8.2,y1=0.32,lwd=1)
text(8.55,0.18,"B")

lines(bo.surv.cl,type="o",lwd=lwds,col=cols[1],pch=20,cex=1.5,lty=ltys[1])
lines(bo.surv.eg,type="o",lwd=lwds,col=cols[2],pch=20,cex=1.5,lty=ltys[2])
lines(bo.surv.sh,type="o",lwd=lwds,col=cols[3],pch=20,cex=1.5,lty=ltys[3])
lines(bo.surv.ma,type="o",lwd=lwds,col=cols[4],pch=20,cex=1.5,lty=ltys[4])

for(i in 2:8){
  segments(x0=i,y0=(bo.surv.cl[i]-bo.se.cl[i]),x1=i,y1=(bo.surv.cl[i]+bo.se.cl[i]),col='black',lwd=1)  
  segments(x0=i-0.1,y0=(bo.surv.cl[i]-bo.se.cl[i]),x1=i+0.1,y1=(bo.surv.cl[i]-bo.se.cl[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.cl[i]+bo.se.cl[i]),x1=i+0.1,y1=(bo.surv.cl[i]+bo.se.cl[i]),col='black',lwd=1)
  
  segments(x0=i,y0=(bo.surv.eg[i]-bo.se.eg[i]),x1=i,y1=(bo.surv.eg[i]+bo.se.eg[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.eg[i]-bo.se.eg[i]),x1=i+0.1,y1=(bo.surv.eg[i]-bo.se.eg[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.eg[i]+bo.se.eg[i]),x1=i+0.1,y1=(bo.surv.eg[i]+bo.se.eg[i]),col='black',lwd=1)
  
  segments(x0=i,y0=(bo.surv.sh[i]-bo.se.sh[i]),x1=i,y1=(bo.surv.sh[i]+bo.se.sh[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.sh[i]-bo.se.sh[i]),x1=i+0.1,y1=(bo.surv.sh[i]-bo.se.sh[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.sh[i]+bo.se.sh[i]),x1=i+0.1,y1=(bo.surv.sh[i]+bo.se.sh[i]),col='black',lwd=1)
  
  segments(x0=i,y0=(bo.surv.ma[i]-bo.se.ma[i]),x1=i,y1=(bo.surv.ma[i]+bo.se.ma[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.ma[i]-bo.se.ma[i]),x1=i+0.1,y1=(bo.surv.ma[i]-bo.se.ma[i]),col='black',lwd=1)
  segments(x0=i-0.1,y0=(bo.surv.ma[i]+bo.se.ma[i]),x1=i+0.1,y1=(bo.surv.ma[i]+bo.se.ma[i]),col='black',lwd=1)
  
}

points(bo.surv.cl,col=cols[1],pch=20,cex=1.5)
points(bo.surv.eg,col=cols[2],pch=20,cex=1.5)
points(bo.surv.sh,col=cols[3],pch=20,cex=1.5)
points(bo.surv.ma,col=cols[4],pch=20,cex=1.5)

legend('topright',lwd=1,col=c(cols[3],cols[4],cols[2],cols[1]),pch=20,lty=c(1,2,2,1),
       legend=c('Shelterwood','Forest','Clearcut Edge','Clearcut Interior'))


par(mar = c(0,3,1,1) + 0.1)

plot(wo.surv.cl,type="o",lwd=1,ylim=c(0,1),xaxt='n',xlab="Time",ylab="",xlim=c(1,8.5),
     col=cols[1],pch=20,cex=1.5)
text(2,0.1,'White Oak',cex=1.3)
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14'))

text(8.55,0.42,"A")
segments(x0=8.3,y0=0.36,x1=8.3,y1=0.48,lwd=1)
segments(x0=8.3,y0=0.36,x1=8.2,y1=0.36,lwd=1)
segments(x0=8.3,y0=0.48,x1=8.2,y1=0.48,lwd=1)

lines(wo.surv.cl,type="o",lwd=lwds,col=cols[1],pch=20,cex=1.5,lty=ltys[1])
lines(wo.surv.eg,type="o",lwd=lwds,col=cols[2],pch=20,cex=1.5,lty=ltys[2])
lines(wo.surv.sh,type="o",lwd=lwds,col=cols[3],pch=20,cex=1.5,lty=ltys[3])
lines(wo.surv.ma,type="o",lwd=lwds,col=cols[4],pch=20,cex=1.5,lty=ltys[4])

for(i in 2:8){
  segments(x0=i,y0=(wo.surv.cl[i]-wo.se.cl[i]),x1=i,y1=(wo.surv.cl[i]+wo.se.cl[i]),col="black",lwd=1)  
  segments(x0=i-0.1,y0=(wo.surv.cl[i]-wo.se.cl[i]),x1=i+0.1,y1=(wo.surv.cl[i]-wo.se.cl[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.cl[i]+wo.se.cl[i]),x1=i+0.1,y1=(wo.surv.cl[i]+wo.se.cl[i]),col="black",lwd=1)
  
  segments(x0=i,y0=(wo.surv.eg[i]-wo.se.eg[i]),x1=i,y1=(wo.surv.eg[i]+wo.se.eg[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.eg[i]-wo.se.eg[i]),x1=i+0.1,y1=(wo.surv.eg[i]-wo.se.eg[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.eg[i]+wo.se.eg[i]),x1=i+0.1,y1=(wo.surv.eg[i]+wo.se.eg[i]),col="black",lwd=1)
  
  segments(x0=i,y0=(wo.surv.sh[i]-wo.se.sh[i]),x1=i,y1=(wo.surv.sh[i]+wo.se.sh[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.sh[i]-wo.se.sh[i]),x1=i+0.1,y1=(wo.surv.sh[i]-wo.se.sh[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.sh[i]+wo.se.sh[i]),x1=i+0.1,y1=(wo.surv.sh[i]+wo.se.sh[i]),col="black",lwd=1)
  
  segments(x0=i,y0=(wo.surv.ma[i]-wo.se.ma[i]),x1=i,y1=(wo.surv.ma[i]+wo.se.ma[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.ma[i]-wo.se.ma[i]),x1=i+0.1,y1=(wo.surv.ma[i]-wo.se.ma[i]),col="black",lwd=1)
  segments(x0=i-0.1,y0=(wo.surv.ma[i]+wo.se.ma[i]),x1=i+0.1,y1=(wo.surv.ma[i]+wo.se.ma[i]),col="black",lwd=1)
  
}

points(wo.surv.cl,col=cols[1],pch=20,cex=1.5)
points(wo.surv.eg,col=cols[2],pch=20,cex=1.5)
points(wo.surv.sh,col=cols[3],pch=20,cex=1.5)
points(wo.surv.ma,col=cols[4],pch=20,cex=1.5)

mtext('Time',side=1,line=2.5,outer=TRUE, at=c(0.55))
mtext('Proportion of Seedlings Surviving',side=2,line=-1.5,outer=TRUE)
dev.off()
#Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.18/bin/gswin64c.exe")
#embed_fonts("../dissertation/figures/fig4-1.pdf")