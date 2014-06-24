###############################################################
##Seedling Survival Simulator
##Shiny Application
###############################################################

#Load required libraries
require(shiny)

load("/home/kkellner/Analysis/seedling_survival/shiny-seedsurv/survoutput.Rda")
load("/home/kkellner/Analysis/seedling_survival/shiny-seedsurv/growthoutput.Rda")

means <- surv.output$means

means2 <- growth.output$means

sim <- function(canopy,distance,aspect,start.height,browse,season,comp,herb,elapsed,sprout){
  
  #Single realization
  site.mean <- means$grand.mean
  
  canopy <- (canopy - 45.509 ) / 40.5605 
    
  elapsed <- (elapsed - 483.5429) / 354.6312
  
  plot.pred <- site.mean + means$b.canopy*canopy + means$b.distance*distance + means$b.aspect*aspect
  
  plot.mean <- plot.pred
  
  seed.pred.b0 <- (plot.mean + means$b.species*0 + means$b.age*0 + means$b.height*start.height
              + means$b.browse*browse + means$b.season*season + means$b.comp*comp + means$b.herb*herb
              + means$b.elapsed*elapsed + means$b.sprout*sprout)
  
  seed.pred.b1 <- (plot.mean + means$b.species*0 + means$b.age*1 + means$b.height*start.height
              + means$b.browse*browse + means$b.season*season + means$b.comp*comp + means$b.herb*herb
              + means$b.elapsed*elapsed + means$b.sprout*sprout)
  
  seed.pred.w0 <- (plot.mean + means$b.species*1 + means$b.age*0 + means$b.height*start.height
              + means$b.browse*browse + means$b.season*season + means$b.comp*comp + means$b.herb*herb
              + means$b.elapsed*elapsed + means$b.sprout*sprout)
  
  seed.pred.w1 <- (plot.mean + means$b.species*1 + means$b.age*1 + means$b.height*start.height
              + means$b.browse*browse + means$b.season*season + means$b.comp*comp + means$b.herb*herb
              + means$b.elapsed*elapsed + means$b.sprout*sprout)
  
  seed.p.b0 <- exp(seed.pred.b0) / (1 + exp(seed.pred.b0))
  seed.p.b1 <- exp(seed.pred.b1) / (1 + exp(seed.pred.b1))
  seed.p.w0 <- exp(seed.pred.w0) / (1 + exp(seed.pred.w0))
  seed.p.w1 <- exp(seed.pred.w1) / (1 + exp(seed.pred.w1))
  
  #Full 7 sample period

  prob.array <- matrix(NA,ncol=6,nrow=4)
  elap <- c(-1.36,-1.11,-0.497,-0.044,0.500,0.9346,1.5691)
  sea <- c(1,0,1,0,1,0,1)
  sp <- c(0,0,1,1)
  ag <- c(0,1,0,1)
  
  for (i in 1:6){
    for (j in 1:4){      
      pr <- (plot.mean + means$b.species*sp[j] + means$b.age*ag[j] + means$b.height*start.height
             + means$b.browse*browse + means$b.season*sea[i] + means$b.comp*comp + means$b.herb*herb
             + means$b.elapsed*elap[i] + means$b.sprout*sprout)
      pr.tr <- exp(pr) / (1 + exp(pr))
      if(i>1){prob.array[j,i] <- pr.tr*prob.array[j,(i-1)]
      } else {prob.array[j,i] <- pr.tr}
    }
  }
  
  ################################################################################
  
  plot.pred <- (means2$grand.mean + means2$b.canopy*canopy + means2$b.distance*distance 
                + means2$b.aspect*aspect)
  
  plot.mean <- plot.pred
  
  seed.gr.b0 <- (plot.mean + means2$b.species*0 + means2$b.age*0 + means2$b.height*start.height
                   + means2$b.browse*browse + means2$b.comp*comp + means2$b.herb*herb
                   + means2$b.sprout*sprout)
  
  seed.gr.b1 <- (plot.mean + means2$b.species*0 + means2$b.age*1 + means2$b.height*start.height
                 + means2$b.browse*browse + means2$b.comp*comp + means2$b.herb*herb
                 + means2$b.sprout*sprout)
  
  seed.gr.w0 <- (plot.mean + means2$b.species*1 + means2$b.age*0 + means2$b.height*start.height
                 + means2$b.browse*browse + means2$b.comp*comp + means2$b.herb*herb
                 + means2$b.sprout*sprout)
  
  seed.gr.w1 <- (plot.mean + means2$b.species*1 + means2$b.age*1 + means2$b.height*start.height
                 + means2$b.browse*browse + means2$b.comp*comp + means2$b.herb*herb
                 + means2$b.sprout*sprout)
  
  #################################################################################
  
  return(list(seed.p.b0=seed.p.b0,seed.p.b1=seed.p.b1,
              seed.p.w0=seed.p.w0,seed.p.w1=seed.p.w1,prob.array=prob.array,
              seed.gr.b0=seed.gr.b0,seed.gr.b1=seed.gr.b1,
              seed.gr.w0=seed.gr.w0,seed.gr.w1=seed.gr.w1))
  
}

shinyServer(function(input, output) {
  
  runsim = reactive({
    sim(input$canopy,input$distance,as.numeric(input$aspect),input$start.height,
        as.numeric(input$browse),as.numeric(input$season),input$comp,input$herb,input$elapsed,
        as.numeric(input$sprout))
  })
  
output$plot = renderPlot({
  run = as.numeric(runsim()[1:4])
  barplot(run,main="Single Sample Period",names=c('Black 0-0','Black 1-0','White 0-0','White 1-0'),ylab="Probability of Survival",ylim=c(0,1),
          xlab="Seedling Species/Age",col=c('red','blue','orange','green'))
})

output$fullplot = renderPlot({
  prob.array = runsim()$prob.array
  plot(prob.array[1,],type="l",col='red',ylim=c(0,1),ylab="Proportion Predicted to Survive",xlab="Sample Period",
       lwd=2,xaxt="n",main="Full Sample Period")
  lines(prob.array[2,],type="l",col="blue",lwd=2)
  lines(prob.array[3,],type="l",col="orange",lwd=2)
  lines(prob.array[4,],type="l",col="green",lwd=2)
  legend(5,.9,legend=c('Black 0-0','Black 1-0','White 0-0','White 1-0'),col=c('red','blue','orange','green'),
         lwd=2)
  axis(side=1,at=1:6,labels=c("Oct11",'May12','Oct12','May13','Oct13','May14'))
})

output$growthplot = renderPlot({
  run = as.numeric(runsim()[6:9])
  barplot(run,names=c('Black 0-0','Black 1-0','White 0-0','White 1-0'),ylab="Expected Growth Increment (cm)",ylim=c(0,9),
          xlab="Seedling Species/Age",col=c('red','blue','orange','green'))
})

#End shiny-server code
})

