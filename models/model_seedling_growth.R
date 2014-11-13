
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,site.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[plot.sitecode[i]] 
                  + b.aspect*aspect[i]
                  + b.edge*edge[i] + b.harvest*harvest[i] + b.shelter*shelter[i]
                  
  }
  
  for (i in 1:nseedlings){
    
    seed.mean[i] ~ dnorm(seed.pred[i], seed.tau)
    seed.pred[i] <- plot.mean[seed.plotcode[i]] + b.species*species[i]
    
    for (j in 1:nsamples[i]){
      
      growth[i,j] ~ dnorm(mu[i,j],obs.tau)
      
      mu[i,j] <- seed.mean[i] 
                      + b.browse*browse[i,j]
                      + b.comp*comp[seed.plotcode[i],j] 
                      + b.sprout*is.sprout[i,j]
                      
      
      res[cucount[i,j]] <- growth[i,j] - mu[i,j]
      sqres[cucount[i,j]] <- pow(res[cucount[i,j]],2)
      growth.new[i,j] ~ dnorm(mu[i,j],obs.tau)
      res.new[cucount[i,j]] <- growth.new[i,j] - mu[i,j]
      sqres.new[cucount[i,j]] <- pow(res.new[cucount[i,j]],2)
    }
  }
  
  #diff.13.12 <- b.y13 - b.y12
  #diff.14.13 <- b.y14 - b.y13
  #diff.14.12 <- b.y14 - b.y12
  
  fit <- sum(sqres[])
  fit.new <- sum(sqres.new[])
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  site.tau <- pow(site.sd,-2)
  site.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  seed.tau <- pow(seed.sd,-2)
  seed.sd ~ dunif(0,100)
  
  obs.tau <- pow(obs.sd,-2)
  obs.sd ~ dunif(0,100)
  
  b.comp ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)  
  b.species ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  
}