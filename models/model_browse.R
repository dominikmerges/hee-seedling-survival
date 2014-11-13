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
    + b.exclude*exclude[i]
    
  }
  
  for (i in 1:nseedlings){
    
    seed.mean[i] ~ dnorm(seed.pred[i], seed.tau)
    seed.pred[i] <- plot.mean[seed.plotcode[i]] + b.species*species[i]
    
    for (j in 1:nsamples[i]){
    
      #Note: for some reason, rjags cannot set a trace monitor for mu (obviously not a good idea anyway since it's huge)
      #You can subset though? Strange.
      mu[i,j] <- seed.mean[i] 
              + b.comp*comp[seed.plotcode[i],j]
              + b.rcd*rcd[i,j]
    
      logit(Q[i,j,1]) <- tau[1,j] - mu[i,j]
      p[i,j,1] <- Q[i,j,1]
    
      for (k in 2:3){
        logit(Q[i,j,k]) <- tau[k,j] - mu[i,j]
        p[i,j,k] <- Q[i,j,k] - Q[i,j,k-1]  
      }
    
      p[i,j,4] <- 1 - Q[i,j,3]
      browse[i,j] ~ dcat(p[i,j,1:4])  
      
      #Expected value
      ev[i,j] <- 1*p[i,j,1] + 2*p[i,j,2] + 3*p[i,j,3] + 4*p[i,j,4]
      
      #Absolute residual for real datapoint
      res[cucount[i,j]] <- abs(browse[i,j] - ev[i,j])
      
      #Simulate new datapoint
      browse.new[i,j] ~ dcat(p[i,j,1:4])
      
      #Absolute residual for simulated datapoint
      res.new[cucount[i,j]] <- abs(browse.new[i,j] - ev[i,j])
      
  }}
  
  #Threshold priors
  for (j in 1:8){
    for(k in 1:3){
      tau0[k,j] ~ dnorm(0,.01)
    }
    tau[1:3,j] <- sort(tau0[1:3,j]) ## JAGS only, not in WinBUGS!
  }
  
  #Derived quantities
  
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
  #Priors
  
  grand.mean ~ dunif(-100,100)
  
  site.tau <- pow(site.sd,-2)
  site.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  seed.tau <- pow(seed.sd,-2)
  seed.sd ~ dunif(0,100)
  
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)
  b.species ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  b.comp ~ dnorm(0,0.01)
  #b.season ~ dnorm(0,0.01)
  b.exclude ~ dnorm(0,0.01)

  
}