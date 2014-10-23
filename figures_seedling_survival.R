#Survival Figure Species*Plant Age
bo = surv[species==0&age==1,2:8]
wo = surv[species==1&age==1,2:8]

bo.surv = vector(length=8)
bo.surv[1] <- 1
for (i in 2:8){
  bo.surv[i] = sum(bo[,(i-1)],na.rm=TRUE) / dim(bo)[1]
}

wo.surv = vector(length=8)
wo.surv[1] <- 1
for (i in 2:8){
  wo.surv[i] = sum(wo[,(i-1)],na.rm=TRUE) / dim(wo)[1]
}

bo0 = surv[species==0&age==0,2:8]
wo0 = surv[species==1&age==0,2:8]

bo.surv0 = vector(length=8)
bo.surv0[1] <- 1
for (i in 2:8){
  bo.surv0[i] = sum(bo0[,(i-1)],na.rm=TRUE) / dim(bo0)[1]
}

wo.surv0 = vector(length=8)
wo.surv0[1] <- 1
for (i in 2:8){
  wo.surv0[i] = sum(wo0[,(i-1)],na.rm=TRUE) / dim(wo0)[1]
}

plot(bo.surv,type="l",lwd=3,ylim=c(0,1),xaxt='n',xlab="Time",ylab="Proportion Surviving")
axis(1,at=c(1:8),labels=c('S11','F11','S12','F12','S13','F13','S14','F14'))
lines(wo.surv,type="l",lwd=3,col='gray')
lines(bo.surv0,type="l",lwd=3,lty=2)
lines(wo.surv0,type="l",lwd=3,lty=2,col='gray')
legend(1,0.3,lwd=3,lty=c(2,2,1,1),col=c('black','gray','black','gray'),
       legend=c('Black Oak 0-0','White Oak 0-0','Black Oak 1-0','White Oak 1-0'))
