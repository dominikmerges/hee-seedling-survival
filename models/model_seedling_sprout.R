
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,site.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[plot.sitecode[i]] 
                  + b.edge*edge[i] + b.harvest*harvest[i] + b.shelter*shelter[i]
                  #+ b.aspect*aspect[i]
                  
  }
  
  for (i in 1:nseedlings){
      
    sprouted[i] ~ dbern(psi[i])
      
    logit(psi[i]) <- plot.mean[seed.plotcode[i]] 
                      + b.species*species[i]
                      + b.rcd*rcd[i]
                      + b.browse*browse[i]
                      + b.comp*comp[i]
      
      res[i] <- abs(sprouted[i] - psi[i])
      sprouted.new[i] ~ dbern(psi[i])
      res.new[i] <- abs(sprouted.new[i] - psi[i])
      
  }
  
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
  #Priors  
  grand.mean ~ dunif(-100,100)
  site.tau <- pow(site.sd,-2)
  site.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)
  b.comp ~ dnorm(0,0.01)
  
  b.species ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
  
}