
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
   
    seed.mean[i] ~ dnorm(seed.pred[i], seed.tau)
    seed.pred[i] <- plot.mean[seed.plotcode[i]]
    
    for (j in 2:nsamples[i]){
      
      surv[i,j] ~ dbern(psi[i,j])
      
      psi[i,j] <- mu[i,j]*surv[i,j-1]
      
      logit(mu[i,j]) <- seed.mean[i]
                      #plot.mean[seed.plotcode[i]]
                      + b.rcd*rcd[i,j-1]
                      + b.sprout*is.sprout[i,j-1]
                      + b.browse*browse[i,j-1] 
                      + b.season*season[j] 
                      #+ b.comp*comp[seed.plotcode[i],j]
                      + b.comp*stem.comp[i,j]
                      + b.elapsed*elapsed[j]
                      #+ b.light*light[seed.plotcode[i],j]
                      #+ b.drought*drought[j]
                      #+ b.lt.elap*light[seed.plotcode[i],j]*elapsed[j]
                      #+ b.lt.elap*light[seed.plotcode[i],j]*drought[j]
      
                      #+ b.harvest_time*harvest[seed.plotcode[i]]*elapsed[j]
                      #+ b.edge_time*edge[seed.plotcode[i]]*elapsed[j]
                      #+ b.shelter_time*shelter[seed.plotcode[i]]*elapsed[j]
                      #+ b.harvest_browse*harvest[seed.plotcode[i]]*browse[i,j-1]
                      #+ b.edge_browse*edge[seed.plotcode[i]]*browse[i,j-1]
                      #+ b.shelter_browse*shelter[seed.plotcode[i]]*browse[i,j-1]
                      #+ b.harvest_comp*harvest[seed.plotcode[i]]*comp[seed.plotcode[i],j]
                      #+ b.edge_comp*edge[seed.plotcode[i]]*comp[seed.plotcode[i],j]
                      #+ b.shelter_comp*shelter[seed.plotcode[i]]*comp[seed.plotcode[i],j]
                      #+ b.browse_comp*browse[i,j-1]*comp[seed.plotcode[i],j]
                      #+ b.sprout_time*is.sprout[i,j-1]*sprout.time[i,j-1]
                      
      
      res[cucount[i,j]] <- abs(surv[i,j] - psi[i,j])
      surv.new[i,j] ~ dbern(psi[i,j])
      res.new[cucount[i,j]] <- abs(surv.new[i,j] - psi[i,j])
      
    }
  }
  
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
  b.edge_harvest <- b.edge - b.harvest
  b.edge_shelter <- b.edge - b.shelter
  b.shelter_harvest <- b.shelter - b.harvest
  
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
  #b.light ~ dnorm(0,0.01)
  #b.lt.elap ~ dnorm(0,0.01)
  #b.drought ~ dnorm(0,0.01)
  
  b.comp ~ dnorm(0,0.01)
  b.aspect ~ dnorm(0,0.01)
  b.elapsed ~ dnorm(0,0.01)  
  b.browse ~ dnorm(0,0.01)
  b.rcd ~ dnorm(0,0.01)
  b.season ~ dnorm(0,0.01)
  b.sprout ~ dnorm(0,0.01)
  
  #b.harvest_time ~ dnorm(0,0.01)
  #b.edge_time ~ dnorm(0,0.01)
  #b.shelter_time ~ dnorm(0,0.01)
  
  #b.harvest_browse ~ dnorm(0,0.01)
  #b.edge_browse ~ dnorm(0,0.01)
  #b.shelter_browse ~ dnorm(0,0.01)
  
  #Non-significant
  #b.harvest_comp ~ dnorm(0,0.01)
  #b.edge_comp ~ dnorm(0,0.01)
  #b.shelter_comp ~ dnorm(0,0.01)
  #b.browse_comp ~ dnorm(0,0.01)
  #b.sprout_time ~ dnorm(0,0.01)
  
}