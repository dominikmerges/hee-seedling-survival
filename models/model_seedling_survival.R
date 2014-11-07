
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,site.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[plot.sitecode[i]] 
                  + b.edge*edge[i]
                  + b.harvest*harvest[i]
                  + b.shelter*shelter[i]
                  + b.aspect*aspect[i]
                  
  }
  
  for (i in 1:nseedlings){
    for (j in 2:nsamples[i]){
      
      surv[i,j] ~ dbern(psi[i,j])
      
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- plot.mean[seed.plotcode[i]] 
                      + b.species*species[i] 
                      + b.rcd*rcd[i,j-1]
                      + b.browse*browse[i,j-1] + b.season*season[j] 
                      + b.comp*comp[seed.plotcode[i],j] 
                      + b.elapsed*elapsed[seed.sitecode[i],j] + b.sprout*is.sprout[i,j-1]
      
      res[cucount[i,j]] <- abs(surv[i,j] - psi[i,j])
      surv.new[i,j] ~ dbern(psi[i,j])
      res.new[cucount[i,j]] <- abs(surv.new[i,j] - psi[i,j])
      
    }
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
  b.aspect ~ dnorm(0,0.01)
  b.elapsed ~ dnorm(0,0.01)  
  b.species ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  b.season ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  
}