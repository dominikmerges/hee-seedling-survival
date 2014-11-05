
model {
  
  #Likelihood
  
  for (i in 1:nsites){
    site.mean[i] ~ dnorm(grand.mean,site.tau)
  }
  
  for (i in 1:nplots){
    plot.mean[i] ~ dnorm(plot.pred[i], plot.tau)
    plot.pred[i] <- site.mean[plot.sitecode[i]] 
                  #+ b.canopy*canopy[i]
                  #+ b.distance*distance[i] 
                  #+ b.distance2*distance2[i]
                  + b.aspect*aspect[i]
                  + b.edge*edge[i] + b.harvest*harvest[i] + b.shelter*shelter[i]
                  
  }
  
  for (i in 1:nseedlings){
    for (j in 1:nsamples[i]){
      
      growth[i,j] ~ dnorm(mu[i,j],ind.tau)
      
      mu[i,j] <- plot.mean[seed.plotcode[i]] 
                      + b.species*species[i] 
                      #+ b.age*age[i] 
                      #+ b.height*start.height[i]
                      #+ b.rcd*rcd[i,j]
                      + b.browse*browse[i,j]
                      + b.comp*comp[seed.plotcode[i],j] 
                      #+ b.herb*herb[seed.plotcode[i],j]
                      + b.sprout*is.sprout[i,j]
                      
      
      res[cucount[i,j]] <- growth[i,j] - mu[i,j]
      sqres[cucount[i,j]] <- pow(res[cucount[i,j]],2)
      growth.new[i,j] ~ dnorm(mu[i,j],ind.tau)
      res.new[cucount[i,j]] <- growth.new[i,j] - mu[i,j]
      sqres.new[cucount[i,j]] <- pow(res.new[cucount[i,j]],2)
    }
  }
  
  fit <- sum(sqres[])
  fit.new <- sum(sqres.new[])
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  site.tau <- pow(site.sd,-2)
  site.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  ind.tau <- pow(ind.sd,-2)
  ind.sd ~ dunif(0,100)
  
  #b.herb ~ dnorm(0,0.01)
  #b.canopy ~ dnorm(0,0.01)
  b.comp ~ dnorm(0,0.01)
  #b.distance ~ dnorm(0,0.01)
  #b.distance2 ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)
  
  b.species ~ dnorm(0,0.01)
  #b.age ~ dnorm(0,0.01)
  b.browse ~ dnorm(0,0.01)
  #b.height ~ dnorm(0,0.01)
  #b.rcd ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  
}