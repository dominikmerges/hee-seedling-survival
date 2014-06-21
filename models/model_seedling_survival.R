
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
    for (j in 2:nsamples[i]){
      
      surv[i,j] ~ dbern(psi[i,j])
      
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- plot.mean[seed.plotcode[i]] 
                      + b.species*species[i] + b.age*age[i] + b.height*start.height[i]
                      + b.browse*browse[i,j-1] + b.season*season[j] 
                      + b.comp*comp[seed.plotcode[i],j] + b.herb*herb[seed.plotcode[i],j]
                      + b.elapsed*elapsed[seed.sitecode[i],j]
      
    }
  }
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  grand.tau <- pow(grand.sd,-2)
  grand.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  b.herb ~ dnorm(0,0.01)
  b.canopy ~ dnorm(0,0.01)
  b.comp ~ dnorm(0,0.01)
  b.distance ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.elapsed ~ dnorm(0,0.01)
  
  b.species ~ dnorm(0,0.01)
  b.age ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
  b.height ~ dnorm(0,0.01)
  b.season ~ dnorm(0,0.01)
  
}