
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,grand.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[plot.sitecode[i]] 
                  + b.canopy*canopy[i]
                  + b.distance*distance[i] + b.aspect*aspect[i]
                  
  }
  
  for (i in 1:nseedlings){
      
    sprouted[i] ~ dbern(psi[i])
      
    logit(psi[i]) <- plot.mean[seed.plotcode[i]] 
                      + b.species*species[i] + b.age*age[i] + b.height*start.height[i]
      
      res[i] <- abs(sprouted[i] - psi[i])
      sprouted.new[i] ~ dbern(psi[i])
      res.new[i] <- abs(sprouted.new[i] - psi[i])
      
  }
  
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  grand.tau <- pow(grand.sd,-2)
  grand.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  b.canopy ~ dnorm(0,0.01)
  b.distance ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  
  b.species ~ dnorm(0,0.01)
  b.age ~ dnorm(0,0.01)
  b.height ~ dnorm(0,0.01)
  
}