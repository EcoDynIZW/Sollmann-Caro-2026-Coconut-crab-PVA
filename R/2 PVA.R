################################################################################
## calculate pop size for Unguja ###############################################

## set working directory to repository dorictory

library(tmvtnorm)
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

## survival from random effect CJS model

##survival estimates - requires running the CJS model in nimble
## and saving output - see script 1
phi1<-read_xlsx('Survival_CJS2v.xlsx')
phi1<-as.data.frame(phi1)

phi1avg<-phi1[phi1$Sex == 'Average', 'Mean']

##index for sites with specific estimates
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
                   R=NA,
                   T=NA)

for(iter in 1:niter){
  
  lamr<-lam$Trend
  phir<-phi1avg

  rran<-lamr-phir
  #   
  N<-R<-matrix(NA, Tt, nsites)
  N[1,]<-round(threats$N[threats$Area %in% keep])

  for (t in 2:Tt){
    s<-rbinom(nsites, N[t-1 ,],phir)#
    R[t,]<-rpois(nsites,N[t-1 ,]*rran)
    N[t,]<-s+R[t,]
  }
  sub.df<-data.frame(site=rep(lam$Site, each=Tt),
                     N=as.vector(N),
                     iter=iter,
                     R=as.vector(R),
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
                     R=NA,
                     T=NA)
  
  for(iter in 1:niter){
    
    ##for all sites, draw random correlated values for lam and phi
    ## truncate to observed min and max from 9 good sites minus Chumbe
    ##from these, calculate r
    ## exception: 9 sites with specific estimates
    
    inp<-rtmvnorm(nrow(threats)-length(keep),
                  mean=c(mean(phi1avg[-which(keep %in% c(9))]),
                         mean(1)),#lam$Trend[-which(keep %in% c(9))])),
                  sigma=cov(cbind(phi1avg[-which(keep %in% c(9))],
                                  lam$Trend[-which(keep %in% c(9))])),
                  lower=c(min(phi1avg),min(lam$Trend)),
                  upper=c(max(phi1avg[-which(keep %in% c(9))]),
                          max(lam$Trend[-which(keep %in% c(9))])))
    
    lamr<-phir<-rep(NA, nrow(threats))
    lamr[pmatch(keep, threats$Area)]<-lam$Trend
    lamr[is.na(lamr)]<-inp[,2]
    phir[pmatch(keep, threats$Area)]<-phi1avg
    phir[is.na(phir)]<-inp[,1]
    rran<-lamr-phir
    #   
    ##starting populations
    N<-R<-matrix(NA, Tt, nsites)
    N[1,]<-round(threats$N)

    ##iterate through years
    for (t in 2:Tt){
      s<-rbinom(nsites, N[t-1 ,],phir)#
      R[t,]<-rpois(nsites,N[t-1 ,]*rran)
      N[t,]<-s+R[t,]
    }
    ##keep track of generated population sizes
    sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                       N=as.vector(N),
                       iter=iter,
                       R=as.vector(R),
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

scen<-c('certain', 'likely', 'possible')
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
  ## calculate proportion habitat retained, which is multiplied with survival
  p.hot<-1-bh
  
  ## annual loss (ag, housing combined) starting in year 2
  ## compound annual loss rate
  ## if this is applied over 9 years, the total loss is achieved
  p.hab<-(1-(ba+bho))^(1/9)
  

  ## which sites experience exploitation? 
  ## 1-exploit is multiplied with survival AFTER habitat loss (code below)
  is.exp<-as.numeric(threats$exploitation %in% indc[1:sc])

  ##keep track of generated population sizes
  out.df2<-data.frame(site=NA,
                      N=NA,
                      iter=NA,
                      R=NA,
                      T=NA,
                      Scenario=NA)
  
 #keep track of expected loss of inds due to hotel, housing and ag, exploitation
 nlost<-nlost.ha<-nexploit<-array(0, c(niter,nsites)) # lost due to ag, housing
 
 ##loop through repeat iterations
  for(iter in 1:niter){
    
    ##generate random exploitation level between 10 and 20%
    exploit<-runif(1, 0.1, 0.2)
    
    #as in status quo above, generate/set survival, lambda, r
    inp<-rtmvnorm(nrow(threats)-length(keep),
                  mean=c(mean(phi1avg[-which(keep %in% c(9))]),
                         mean(1)),#lam$Trend[-which(keep %in% c(9))])),
                  sigma=cov(cbind(phi1avg[-which(keep %in% c(9))],
                                  lam$Trend[-which(keep %in% c(9))])),
                  lower=c(min(phi1avg),min(lam$Trend)),
                  upper=c(max(phi1avg[-which(keep %in% c(9))]),
                          max(lam$Trend[-which(keep %in% c(9))])))
    
    lamr<-phir<-rep(NA, nrow(threats))
    lamr[pmatch(keep, threats$Area)]<-lam$Trend
    lamr[is.na(lamr)]<-inp[,2]
    phir[pmatch(keep, threats$Area)]<-phi1avg
    phir[is.na(phir)]<-inp[,1]
    rran<-lamr-phir
    
    #starting population sizes   
    N<-R<-matrix(NA, Tt, nsites)
    N[1,]<-round(threats$N)
    
    ##generate year of loss hotel, year of exploitation
    ##effects start in interval before chosen year
    ##set sites not experiencing given threat to Tt+1 (does not affect simulations)
    year.loss<- year.exp<-rep(Tt+1, nsites)
    year.loss[p.hot<1]<-sample(2:Tt, sum(p.hot<1), replace=T) 
    year.exp[is.exp==1]<-sample(2:Tt, sum(is.exp), replace=T) 

    ## loop through years
    for (t in 2:Tt){
      
      ##reduction due to habitat loss from housing, ag
      phi.eff<-phir*p.hab
      
      ##calculate expected number of individuals lost due to housing/ag
      ##cumulatively over the 10-year projections
      ##is later split proportionally between the two threats
      nlost.ha[iter,]<-nlost.ha[iter,] + (N[t-1 ,]*(1-p.hab))

      ##if year of hotel loss, reduce survival once
      if(any(year.loss==t)){
        phi.eff[year.loss==t]<-phi.eff[year.loss==t]*p.hot[year.loss==t]

        ##calculate number individuals lost due to habitat loss, cumulative over years
        nlost[iter,year.loss==t]<-nlost[iter,year.loss==t]+ 
          (1-p.hot[year.loss==t])*N[t-1 ,][year.loss==t]
        
      }
      
      ##if after exploitation started, reduce survival in all subsequent years
      if(any(year.exp>=t)){
        phi.eff[ t>=year.exp] <-phi.eff[ t>=year.exp] * (1-exploit)
      
        ##calculate number individuals lost due to exploitation, cumulative over years
        nexploit[iter,t>=year.exp]<-nexploit[iter,t>=year.exp]+
          N[t-1 , t>=year.exp] * exploit
        
      }
      
      s<-rbinom(nsites, N[t-1 ,],phi.eff)
      
      R[t,]<-rpois(nsites,N[t-1 ,]*rran)
      N[t,]<-s+R[t,]
    }
    ##keep track of results
    sub.df<-data.frame(site=rep(threats$Name, each=Tt),
                       N=as.vector(N),
                       iter=iter,
                       R=as.vector(R),
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


