#################################################
##Preliminary Seedling Survival Figures
#################################################

#Read in data via analyis script

#Change this line to select different treatments to plot
survpl1 = survival[survival$trt=="NN",]

B0 = c(mean(survpl1[(survpl1$Age==0&survpl1$Species=="B"),9]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="B"),10]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="B"),11]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="B"),12]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="B"),13]))

W0 = c(mean(survpl1[(survpl1$Age==0&survpl1$Species=="W"),9]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="W"),10]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="W"),11]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="W"),12]),
       mean(survpl1[(survpl1$Age==0&survpl1$Species=="W"),13]))

B1 = c(mean(survpl1[(survpl1$Age==1&survpl1$Species=="B"),9]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="B"),10]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="B"),11]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="B"),12]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="B"),13]))

W1 = c(mean(survpl1[(survpl1$Age==1&survpl1$Species=="W"),9]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="W"),10]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="W"),11]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="W"),12]),
       mean(survpl1[(survpl1$Age==1&survpl1$Species=="W"),13]))

#Change this line to plot different age/species combinations
plot(B0,ylim=c(0,1),ylab="Survival",xlab="Time",pch=19,cex=1.5,main="Herb Comp",xaxt='n')
axis(side=1,at=c(1,2,3,4,5), labels=c('Jul11','Oct11','May12','Oct12','May13'),
     tck=0)
lines(B0)
points(W0,cex=1.5)
lines(W0)
points(B1,pch=17,cex=1.5)
lines(B1)
points(W1,pch=24,cex=1.5)
lines(W1)
legend(4,1,c("0-0 Black","0-0 White","1-0 Black","1-0 White"),pch=c(19,21,17,24))



