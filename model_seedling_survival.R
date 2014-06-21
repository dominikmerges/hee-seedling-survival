
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,grand.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[siteid[i]] + b.herbivory*herbivory[i] + b.competition*competition[i]
    + b.distance*distance[i]
  }
  
  for (i in 1:nseedlings){
    for (j in 2:nsamples){
      
      surv[i,j] ~ dbern(psi[i,j])
      
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- plot.mean[plotid[i]] + b.species*species[i] + b.age*age[i]
      
    }
  }
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  grand.tau <- pow(grand.sd,-2)
  grand.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  b.herbivory ~ dnorm(0,0.01)
  b.competition ~ dnorm(0,0.01)
  b.distance ~ dnorm(0,0.01)
  b.species ~ dnorm(0,0.01)
  b.age ~ dnorm(0,0.01)
  
  
  
  
}