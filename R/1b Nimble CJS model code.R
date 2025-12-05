#### CJS model

cjs.2v<-nimbleCode({
  
  for(i in 1:n){
    
    sex[i]~dbern(psi)
    
    ##detection conditional on alive
    logit(p[i,1:leng[i]])<-a.p +b.eff*(effort[first[i]:last[i]])+bp.sex*sex[i]
    logit(phi[i])<-a.phi[site[i]] + b.sex*sex[i]
    
    for (t in 1:(leng[i]-1)){
      phi.eff[i,t]<-phi[i]^dt[i,first[i]+(t-1)]
    }
    
    obs[i,first[i]:last[i]] ~ dCJS_vv(probSurvive = phi.eff[i,1:(leng[i]-1)], 
                                      probCapture = p[i,1:leng[i]],
                                      len=leng[i])
    
  }
  
  
  #Priors
  psi~dbeta(1,1)
  b.sex~dnorm(0,sd=5)
  
  ## detection parameters
  b.eff~dnorm(0,sd=5)
  bp.sex~dnorm(0,sd=5)
  b.area~dnorm(0,sd=5)

  ##get mean detection intercept on real scale
  mean.p~dbeta(1,1)
  a.p<-log(mean.p/(1-mean.p))
  
  ##same for survival
  mean.phi~dbeta(1,1)
  mu.phi<-log(mean.phi/(1-mean.phi))
  sig.phi~dunif(0,5)
  
  ##draw site level intercept p
  for (jj in 1:nsites){
    a.phi[jj]~dnorm(mu.phi,sd=sig.phi)
  }
  
})


