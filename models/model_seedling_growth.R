#########################################################
##BUGS Code for Hierarchical Oak Seedling Growth Model##
#########################################################

model {
  
  #Likelihood
  
  #Plot mean
  for (i in 1:nsites){
    plot.mean[i] ~ dnorm(grand.mean,plot.tau)
  }
  
  #Subplot mean
  for (i in 1:nplots){
    subplot.mean[i] ~ dnorm(subplot.pred[i], subplot.tau)
    subplot.pred[i] <- plot.mean[plot.sitecode[i]] 
    + b.aspect*aspect[i] #effect of aspect
    + b.edge*edge[i] #effect of harvest treatment
    + b.harvest*harvest[i]
    + b.shelter*shelter[i]
  }
  
  #Seedling mean
  for (i in 1:nseedlings){
    
    #Seedling random effect
    seed.mean[i] ~ dnorm(seed.pred[i], seed.tau)
    seed.pred[i] <- plot.mean[seed.plotcode[i]]
    
    #Iterate over each sampling occasion
    for (j in 1:nsamples[i]){
      
      #Model neglog-transformed data
      growth[i,j] ~ dnorm(mu[i,j],obs.tau)
      
      #Expected growth
      mu[i,j] <- seed.mean[i]
      + b.browse*browse[i,j] #effect of herbivory
      + b.comp*stem.comp[i,j] #effect of competition
      + b.sprout*is.sprout[i,j] #effect of being a resprout
      + b.elapsed*elapsed[j] #effect of elapsed time since planting
      
      #Calculate values for posterior predictive check
      res[cucount[i,j]] <- growth[i,j] - mu[i,j]
      sqres[cucount[i,j]] <- pow(res[cucount[i,j]],2)
      growth.new[i,j] ~ dnorm(mu[i,j],obs.tau)
      res.new[cucount[i,j]] <- growth.new[i,j] - mu[i,j]
      sqres.new[cucount[i,j]] <- pow(res.new[cucount[i,j]],2)
    }
  }
  
  
  #For posterior predictive check
  fit <- sum(sqres[])
  fit.new <- sum(sqres.new[])
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  subplot.tau <- pow(subplot.sd,-2)
  subplot.sd ~ dunif(0,100)
  
  seed.tau <- pow(seed.sd,-2)
  seed.sd ~ dunif(0,100)
  
  obs.tau <- pow(obs.sd,-2)
  obs.sd ~ dunif(0,100)
  
  b.comp ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)  
  b.browse ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  b.elapsed ~ dnorm(0,0.01)
  
}