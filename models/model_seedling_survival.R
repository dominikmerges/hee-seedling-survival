##########################################################
##BUGS Code for Hierarchical Oak Seedling Survival Model##
##########################################################

model {
  
  #Likelihood
  
  #Plot mean
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(grand.mean,plot.tau)
  }
  
  #Subplot mean
  for (i in 1:nsubplots){
    subplot.mean[i] ~ dnorm(subplot.pred[i], subplot.tau)
    subplot.pred[i] <- plot.mean[plot.sitecode[i]] 
    + b.edge*edge[i] #Effect of harvest treatment
    + b.harvest*harvest[i]
    + b.shelter*shelter[i]
    + b.aspect*aspect[i] #Effect of aspect
  }
  
  #Seedling mean
  for (i in 1:nseedlings){
    
    #Iterate over each sampling occasion
    for (j in 2:nsamples[i]){
      
      #Model raw survival data
      surv[i,j] ~ dbern(psi[i,j])
      
      #If seedling is already dead, expected value is 0
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      #Expected value
      logit(mu[i,j]) <- plot.mean[seed.plotcode[i]]
      + b.rcd*rcd[i,j-1] #Effect of root collar diameter
      + b.sprout*is.sprout[i,j-1] #Effect of being a sprout
      + b.browse*browse[i,j-1] #Effect of herbivory
      + b.season*season[j] #Effect of growing season
      + b.comp*stem.comp[i,j] #Effect of competition
      + b.elapsed*elapsed[j] #Effect of elapsed time since planting         
      
      #Calculate values for posterior predictive check
      res[cucount[i,j]] <- abs(surv[i,j] - psi[i,j])
      surv.new[i,j] ~ dbern(psi[i,j])
      res.new[cucount[i,j]] <- abs(surv.new[i,j] - psi[i,j])
      
    }
  }
  
  #For posterior predictive check
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
  #Derived differences between treatments
  b.edge_harvest <- b.edge - b.harvest
  b.edge_shelter <- b.edge - b.shelter
  b.shelter_harvest <- b.shelter - b.harvest
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  subplot.tau <- pow(subplot.sd,-2)
  subplot.sd ~ dunif(0,100)
  
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)
  
  b.comp ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.elapsed ~ dnorm(0,0.01)  
  b.browse ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  b.season ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  
}
