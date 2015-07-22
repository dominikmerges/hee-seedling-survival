
model {
  
  #Likelihood

  for (i in 1:nseedlings){
   
    #seed.mean[i] ~ dnorm(0, seed.tau)
    
    for (j in 2:nsamples[i]){
      
      surv[i,j] ~ dbern(psi[i,j])
      
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- grand.mean #+ seed.mean[i]
                      #+ b.browse*browse[i,j-1]
                      + b.sprout*is.sprout[i,j-1]
                      + b.species*species[i]
                      + b.shade*canopy[seed.plotcode[i]]
                      #+ b.height*st.height[i,j-1]
                      + b.age*age[i,j-1]
                      #+ b.browseht*st.height[i,j-1]*browse[i,j-1]

    }
  }

  #Priors
  
  grand.mean ~ dunif(-100,100)  
  #seed.tau <- pow(seed.sd,-2)
  #seed.sd ~ dunif(0,100)
  
  b.age ~ dnorm(0,0.01)
  b.species ~ dnorm(0,0.01)
  #b.browse ~ dnorm(0,0.01)
  #b.height ~ dnorm(0,0.01)
  b.shade ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  #b.browseht ~ dnorm(0,0.01)
  
}