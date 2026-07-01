################################################################################
## calculate pop size for Unguja ###############################################

## strategy: randomize order of impact
## for each year, always create a full site long vector of
## reductions due to each impact
## then the calculations should be the same for all impacts
## and you can just randomize which impact you use first, secodn, etc
## set working directory to repository dorictory

library(truncnorm)
library(readxl)
library(writexl)
library(abind)
#library(ggplot2)
#library(ggbreak)
#library(RColorBrewer)
#library(scales)
#library(reshape2)

threats<-readRDS('data/threats_processed.rds')
## assign survival probabilities, recruitment rates

## trend from CPUE analyis
lam<-read_xlsx('data/Trend estimates good data sites.xlsx')
lam<-as.data.frame(lam)

##index for sites with specific trend estimates
keep<-c(1,2,3,4,5,6,7,9,20)


##projection settings: 10 years, 500 times
Tt<-10
niter=500

#######################################################################
### project 9 good sites only, for supplement figure ##################
nsites<-length(keep)

set.seed(123)

##prepare data frame to hold output
out.df0<-data.frame(site=NA,
                   N=NA,
                   iter=NA,
                   T=NA)

for(iter in 1:niter){
  
  lamr<-lam$Trend
  #   
  N<-matrix(NA, Tt, nsites)
  N[1,]<-round(threats$N[threats$Area %in% keep])

  for (t in 2:Tt){
    N[t,]<-rpois(nsites,N[t-1 ,]*lamr)
  }
  sub.df<-data.frame(site=rep(lam$Site, each=Tt),
                     N=as.vector(N),
                     iter=iter,
                     T=rep(1:Tt, nsites))
  out.df0<-rbind(out.df0, sub.df)
}

out.df0<-out.df0[-1,]

##save output
saveRDS(out.df0, 'ProjectionsNineGoodSites.rds')


##########################################################################
### Projections all subpopulations, status quo/baseline
### the generated overall trajectory reflects that of just the 9 'good' subpopulations

set.seed(123)
nsites<-nrow(threats)
##prepare data frame to hold output
  out.df<-data.frame(site=NA,
                     N=NA,
                     iter=NA,
                     T=NA)
  
  for(iter in 1:niter){
    
    ##for all sites, draw random values for lam 
    ## truncate to observed min and max from 9 good sites minus Chumbe
    ## exception: 9 sites with specific estimates

    inp<-rtruncnorm(n=nrow(threats)-length(keep),
                  mean=1,
                  sd=sd(lam$Trend[-which(keep %in% c(9))]),
                  a=min(lam$Trend),
                  b=max(lam$Trend[-which(keep %in% c(9))]) )
    
    lamr<-rep(NA, nrow(threats))
    lamr[pmatch(keep, threats$Area)]<-lam$Trend
    lamr[is.na(lamr)]<-inp

    #   
    ##starting populations
    N<-matrix(NA, Tt, nsites)
    N[1,]<-round(threats$N)

    ##iterate through years
    for (t in 2:Tt){
      N[t,]<-rpois(nsites,N[t-1 ,]*lamr)
    }
    ##keep track of generated population sizes
    sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                       N=as.vector(N),
                       iter=iter,
                       T=rep(1:Tt, nsites))
    out.df<-rbind(out.df, sub.df)
  }
  #remove first NA row
  out.df<-out.df[-1,]

  ##add scenario label so can be combined with other projections
  out.df$Scenario<-'status quo'
  ##save output
  saveRDS(out.df, 'ProjectionsStatusQuo.rds')



################################################################################
### future scenarios

##three scenarios

scen<-c('highly likely', 'likely', 'possible')

##indicators in threat data corresponding to the three scenarios
indc<-c('y', 'y?', 'n?')

##keep track of total losses across years for each site and 
##cause
out.list<-lost.list<-exp.list<-lost.list2<-list()


set.seed(123)

for (sc in 1:length(scen)){
  
  ## first, determine who is impacted by what and how for given scenario
  bh<-as.numeric(threats$Hotel %in% indc[1:sc])* (threats$`Hotel%`/100)
  ba<-as.numeric(threats$agriculture %in% indc[1:sc])* (threats$`Agric%`/100)
  bho<-as.numeric(threats$housing %in% indc[1:sc])* (threats$`House%`/100)

  ### one-time effect of habitat loss on survival for hotel construction
  p.hot<-bh
  
  ## annual loss (ag, housing combined) starting in year 2
  ## compound annual loss rate
  ## if this is applied over 9 years, the total loss is achieved
  p.hab<-1-(1-(ba+bho))^(1/9)

  ## which sites experience exploitation? 
  is.exp<-as.numeric(threats$exploitation %in% indc[1:sc])

  ##keep track of generated population sizes
  out.df2<-data.frame(site=NA,
                      N=NA,
                      iter=NA,
                      T=NA,
                      Scenario=NA)
  
 #keep track of expected loss of inds due to hotel, housing and ag, exploitation
 nlost<-nlost.ha<-nexploit<-array(0, c(niter,nsites)) # lost due to ag, housing
 
 ##loop through repeat iterations
  for(iter in 1:niter){
    
    ##generate random exploitation level between 10 and 20%
    exploit<-runif(1, 0.1, 0.2)
    
    #as in status quo above, generate/set survival, lambda, r
    inp<-rtruncnorm(n=nrow(threats)-length(keep),
                    mean=1,
                    sd=sd(lam$Trend[-which(keep %in% c(9))]),
                    a=min(lam$Trend),
                    b=max(lam$Trend[-which(keep %in% c(9))]) )
    
    lamr<-rep(NA, nrow(threats))
    lamr[pmatch(keep, threats$Area)]<-lam$Trend
    lamr[is.na(lamr)]<-inp
    
    #starting population sizes   
    N<-matrix(NA, Tt, nsites)
    N[1,]<-round(threats$N)
    
    ##generate year of loss hotel, year of exploitation
    ##effects start in interval before chosen year
    ##
    ##set sites not experiencing given threat to Tt+1 (does not affect simulations)
    year.loss<- year.exp<-rep(Tt+1, nsites)
    year.loss[p.hot>0]<-sample(2:Tt, sum(p.hot>0), replace=T) 
    year.exp[is.exp==1]<-sample(2:Tt, sum(is.exp), replace=T) 

    ## loop through years
    for (t in 2:Tt){
      
      ##set up threat effects
      ## hab effect: each year, take same proportion of previous year; in a stable
      ## pop, that would equate to x% of the starting pop
      ## because the population can grow/decline due to other causes, the numbers
      ## don't play out like that but the theory is right - I triple checked!!
      hab.eff<-p.hab 
      ##this is necessary because hotel loss is only one year
      hotel.eff<-ifelse(year.loss == t, p.hot, 0)
      exploit.eff<-ifelse(year.exp >= t & year.exp<11, exploit,0)
        
      total.loss<-hab.eff + hotel.eff + exploit.eff
      total.loss[total.loss >1]<-1
      
      lam.eff<-lamr * (1- total.loss)
      nlost.t<-N[t-1, ]*(lamr - lam.eff)
      
      ##calculate expected number of individuals lost
      ##cumulatively over the 10-year projections
      ##housing/ag is later split proportionally

      nlost.ha[iter,total.loss >0]<-nlost.ha[iter,total.loss >0] + 
        nlost.t[total.loss >0] * (hab.eff[total.loss >0]/total.loss[total.loss >0])
        
      ##lost due to hotels
      nlost[iter,total.loss >0]<-nlost[iter,total.loss >0] + 
        nlost.t[total.loss >0] * (hotel.eff[total.loss >0]/total.loss[total.loss >0])
      
      ##lost due to exploitation
      nexploit[iter,total.loss >0]<-nexploit[iter,total.loss >0]+
        nlost.t[total.loss >0] * (exploit.eff[total.loss >0]/total.loss[total.loss >0])

      N[t,]<-rpois(nsites,N[t-1 ,]*lam.eff)
                                              
    }
    
    ##keep track of results
    sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                       N=as.vector(N),
                       iter=iter,
                       T=rep(1:Tt, nsites),
                       Scenario=scen[sc])
    out.df2<-rbind(out.df2, sub.df)
  }

  out.list[[sc]]<-out.df2[-1,]
  lost.list[[sc]]<-nlost
  lost.list2[[sc]]<-nlost.ha
  exp.list[[sc]]<-nexploit
} 

out.df2<-do.call(rbind, out.list)

##save output
saveRDS(out.df2, 'ProjectionsScenarios.rds')
saveRDS(list(lost.list=lost.list, 
             lost.list2=lost.list2,
             exp.list= exp.list),
        'LossesScenarios.rds')


################################################################################
####### SENSITIVITY ANALYSIS ###################################################
### for supplements 

### status quo and future scenarios WITH metapop effect

##proportion of trend affected by metapop effect (ie, recruitment)
pmeta<-seq(0, 0.5, 0.1)

set.seed(123)

sqlist<-list()

for (pp in 1:length(pmeta)){

  ##prepare data frame to hold output
  out.df<-data.frame(site=NA,
                     N=NA,
                     iter=NA,
                     T=NA,
                     pmeta=NA)
  
  for(iter in 1:niter){
    
    ##for all sites, draw random correlated values for lam and phi
    ## truncate to observed min and max from 9 good sites minus Chumbe
    ##from these, calculate r
    ## exception: 9 sites with specific estimates
    
    inp<-rtruncnorm(n=nrow(threats)-length(keep),
                    mean=1,#lam$Trend[-which(keep %in% c(9))])),
                    sd=sd(lam$Trend[-which(keep %in% c(9))]),
                    a=min(lam$Trend),
                    b=max(lam$Trend[-which(keep %in% c(9))]) )
    
    lamr<-rep(NA, nrow(threats))
    lamr[pmatch(keep, threats$Area)]<-lam$Trend
    lamr[is.na(lamr)]<-inp
    
    #   
    ##starting populations
    N<-matrix(NA, Tt, nsites)
    N[1,]<-round(threats$N)
    
    ##iterate through years
    for (t in 2:Tt){
      
      ##metapop effect - reduction in growth rate when metapop declines
      if(t==2){frac=1}else{
        frac<-sum(N[t-1 ,])/sum(N[t-2,])
      }

      lamrt<- (1-pmeta[pp])*lamr + pmeta[pp]*lamr*frac
      N[t,]<-rpois(nsites,N[t-1 ,]*lamrt)
    }
    ##keep track of generated population sizes
    sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                       N=as.vector(N),
                       iter=iter,
                       T=rep(1:Tt, nsites),
                       pmeta=pmeta[pp])
    out.df<-rbind(out.df, sub.df)
  }
  #remove first NA row
  out.df<-out.df[-1,]
  ##add scenario label so can be combined with other projections
  out.df$Scenario<-'status quo'
  sqlist[[pp]]<-out.df
}

sqout<-do.call(rbind, sqlist)

##save output
saveRDS(sqout, 'ProjectionsStatusQuo_meta.rds')



##three future scenarios
scen<-c('highly likely', 'likely', 'possible')
##indicators in threat data corresponding to the three scenarios
indc<-c('y', 'y?', 'n?')


set.seed(123)
fsout<-list()

for (pp in 1:length(pmeta)){
  ##keep track of total losses across years for each site and 
  ##cause
  out.list<-list()
  
  for (sc in 1:length(scen)){
    
    ## first, determine who is impacted by what and how for given scenario
    bh<-as.numeric(threats$Hotel %in% indc[1:sc])* (threats$`Hotel%`/100)
    ba<-as.numeric(threats$agriculture %in% indc[1:sc])* (threats$`Agric%`/100)
    bho<-as.numeric(threats$housing %in% indc[1:sc])* (threats$`House%`/100)
    
    ### one-time effect of habitat loss on survival for hotel construction
    ## calculate proportion habitat retained, which is multiplied with survival
    #p.hot<-1-bh
    p.hot<-bh
    
    ## annual loss (ag, housing combined) starting in year 2
    ## compound annual loss rate
    ## if this is applied over 9 years, the total loss is achieved
    p.hab<-1-(1-(ba+bho))^(1/9)
    
    ## which sites experience exploitation? 
    ## 1-exploit is multiplied with survival AFTER habitat loss (code below)
    is.exp<-as.numeric(threats$exploitation %in% indc[1:sc])
    
    ##keep track of generated population sizes
    out.df2<-data.frame(site=NA,
                        N=NA,
                        iter=NA,
                        T=NA,
                        Scenario=NA,
                        pmeta=NA)
    
    ##loop through repeat iterations
    for(iter in 1:niter){
      
      ##generate random exploitation level between 10 and 20%
      exploit<-runif(1, 0.1, 0.2)
      
      #as in status quo above, generate/set survival, lambda, r
      inp<-rtruncnorm(n=nrow(threats)-length(keep),
                      mean=1,
                      sd=sd(lam$Trend[-which(keep %in% c(9))]),
                      a=min(lam$Trend),
                      b=max(lam$Trend[-which(keep %in% c(9))]) )
      
      lamr<-rep(NA, nrow(threats))
      lamr[pmatch(keep, threats$Area)]<-lam$Trend
      lamr[is.na(lamr)]<-inp
      
      #starting population sizes   
      N<-matrix(NA, Tt, nsites)
      N[1,]<-round(threats$N)
      
      ##generate year of loss hotel, year of exploitation
      ##effects start in interval before chosen year
      ##
      ##set sites not experiencing given threat to Tt+1 (does not affect simulations)
      year.loss<- year.exp<-rep(Tt+1, nsites)
      year.loss[p.hot>0]<-sample(2:Tt, sum(p.hot>0), replace=T) 
      year.exp[is.exp==1]<-sample(2:Tt, sum(is.exp), replace=T) 
      
      ## loop through years
      for (t in 2:Tt){
        
        
        ##metapop effect - reduction in growth rate if metapop declines
        if(t==2){frac=1}else{
          frac<-sum(N[t-1 ,])/sum(N[t-2,])
        }
        lamrt<- (1-pmeta[pp])*lamr + pmeta[pp]*lamr*frac
        
        ##set up threat effects
        ## hab effect: each year, take same proportion of previous year; in a stable
        ## pop, that would equate to x% of the starting pop
        ## because the population can grow/decline due to other causes, the numbers
        ## don't play out like that but the theory is right - I triple checked!!
        hab.eff<-p.hab 
        ##this is necessary because hotel loss is only one year
        hotel.eff<-ifelse(year.loss == t, p.hot, 0)
        exploit.eff<-ifelse(year.exp >= t & year.exp<11, exploit,0)
        
        total.loss<-hab.eff + hotel.eff + exploit.eff
        total.loss[total.loss >1]<-1
        
        lam.eff<-lamrt * (1- total.loss)
        
        N[t,]<-rpois(nsites,N[t-1 ,]*lam.eff)
        
      }
      
      ##keep track of results
      sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                         N=as.vector(N),
                         iter=iter,
                         T=rep(1:Tt, nsites),
                         Scenario=scen[sc], 
                         pmeta=pmeta[pp])
      out.df2<-rbind(out.df2, sub.df)
    }
    
    out.list[[sc]]<-out.df2[-1,]
  } 
  
  fsout[[pp]]<-do.call(rbind, out.list)
  
}

fsout<-do.call(rbind, fsout)

##save output
saveRDS(fsout, 'ProjectionsScenarios_meta.rds')
