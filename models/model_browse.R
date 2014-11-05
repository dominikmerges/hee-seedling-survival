model {
  
  #Likelihood
  
  #for (i in 1:nsites){
  #  site.effect[i] ~ dnorm(0,site.tau)
  #}
  
  for (i in 1:nplots){
    plot.effect[i] ~ dnorm(0, plot.tau)  
  }
  
  for (i in 1:nseedlings){
    
    for (j in 1:nsamples[i]){
    
      #Note: for some reason, rjags cannot set a trace monitor for mu (obviously not a good idea anyway since it's huge)
      #You can subset though? Strange.
      mu[i,j] <- #site.effect[seed.sitecode[i]] 
              plot.effect[seed.plotcode[i]] 
              #+ b.distance*distance[seed.plotcode[i]] 
              #+ b.comp*comp[seed.plotcode[i],j] + b.herb*herb[seed.plotcode[i],j]
              #+ b.age*age[i] + b.height*start.height[i] 
              + b.rcd*rcd[i,j]
              + b.species*species[i]
              + b.edge*edge[seed.plotcode[i]] + b.harvest*harvest[seed.plotcode[i]]
              + b.shelter*shelter[seed.plotcode[i]]
              + b.season*season[j] #not identifiable for some reason
              + b.exclude*exclude[seed.plotcode[i]]
              + b.pellet*pellet[seed.sitecode[i],pindex[j]]
              #+ b.distance2*distance2[seed.plotcode[i]]
    
      logit(Q[i,j,1]) <- tau[1,j] - mu[i,j]
      p[i,j,1] <- Q[i,j,1]
    
      for (k in 2:3){
        logit(Q[i,j,k]) <- tau[k,j] - mu[i,j]
        p[i,j,k] <- Q[i,j,k] - Q[i,j,k-1]  
      }
    
      p[i,j,4] <- 1 - Q[i,j,3]
      browse[i,j] ~ dcat(p[i,j,1:4])
  }}
  
  #Threshold priors
  for (j in 1:8){
    for(k in 1:3){
      tau0[k,j] ~ dnorm(0,.01)
    }
    tau[1:3,j] <- sort(tau0[1:3,j]) ## JAGS only, not in WinBUGS!
  }
  
  #Interpolation  
  #for (i in 1:nsites){
  #  for (j in 1:4){
      
  #    pellet[i,j] ~ dpois(lambda[i,j])
  #    log(lambda[i,j]) <- alpha.lam + beta.dist*site.dist[i] + beta.pelmean*pelmean[i]
  #}}
  
  #Priors
  
  #site.tau <- pow(site.sd,-2)
  #site.sd ~ dunif(0,100)
  
  plot.tau <- pow(plot.sd,-2)
  plot.sd ~ dunif(0,100)
  
  #b.distance ~ dnorm(0,0.01)
  #b.distance2 ~ dnorm(0,0.01)
  b.edge ~ dnorm(0,0.01)
  b.harvest ~ dnorm(0,0.01)
  b.shelter ~ dnorm(0,0.01)
  b.species ~ dnorm(0,0.01)
  #b.age ~ dnorm(0,0.01)
  #b.height ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  #b.comp ~ dnorm(0,0.01)
  b.season ~ dnorm(0,0.01)
  b.pellet ~ dnorm(0,0.01)
  #b.herb ~ dnorm(0,0.01)
  b.exclude ~ dnorm(0,0.01)
  
  #beta.dist ~ dnorm(0,0.01)
  #beta.pelmean ~ dnorm(0,0.01)
  #alpha.lam ~ dnorm(-100,100)
  
}