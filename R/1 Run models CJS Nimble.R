################################################################################
##### Run CJS models in Nimble #################################################
rm(list=ls())

### set working directory to repository directory for all paths to work

library(nimble)
library(nimbleEcology)
library(MCMCvis)
library(readxl)
library(writexl)


source('R/1b Nimble CJS model code.R')

##read in data
dat.list<-readRDS('data/CMR_data.rds')

##from ML CJS model selection: use field person minutes
eff<-dat.list$survey

##adjust for proportion with photograph
effort<-((eff$FieldPersM/1000)*eff$prop)

### subset data for survival to individuals not caught first in last survey

omega5<-dat.list$omega[-which(dat.list$first == dat.list$last), ]
first5<-dat.list$first[-which(dat.list$first == dat.list$last)]
sex5<-dat.list$sex[-which(dat.list$first == dat.list$last)]
last5<-dat.list$last[-which(dat.list$first == dat.list$last)]
nind<-nrow(omega5)
stidx5<-dat.list$stidx[-which(dat.list$first == dat.list$last)]
dmat.surv5<-dat.list$dmat.surv[-which(dat.list$first == dat.list$last),]
lambda5<-dat.list$lambda[-which(dat.list$first == dat.list$last)]


## set up data constants
nind<-length(sex5)
nsites<-length(unique(stidx5))

dat<-list(sex=sex5, obs=omega5)

consts<-list(n=nind,
              first=first5,
              last=last5,
              dt=dmat.surv5,
              nsites=nsites,
              site=stidx5,
              effort=effort,
             leng=last5-first5+1)


### prepare initial values
##sex
sex.in<-rep(NA, nind)
sex.in[is.na(sex5)]<-0

##p(female)
psi.in<-sum(sex5, na.rm=T)/length(sex5)

inits<-function(){list(sex=sex.in,
                       psi=psi.in,
                       mean.p=0.1,
                       mean.phi=0.8, 
                       sig.phi=0.5,
                       # z=z.in,
                       b.sex=runif(1, -1, 0),
                       bp.sex=runif(1, -1, 0),
                       b.eff=runif(1, 0, 1))}

##nodes to monitor
params<-c('psi','mean.p',
          'a.p',
          'mean.phi','sig.phi','a.phi',
          'bp.sex',
          'b.eff',  'b.sex')

## create and compile model
model <- nimbleModel(cjs.2v, constants = consts, 
                     data=dat, inits=inits(), check = FALSE)

cmodel <- compileNimble(model)    

##use block sampling for survival params (improves mixing)
conf.mcmc<-configureMCMC(model, monitors = params, thin=5)

conf.mcmc$removeSamplers(c('mean.phi', 'b.sex', 'a.phi'))
conf.mcmc$addSampler(target = c('mean.phi', 'b.sex', 'a.phi'), type = 'AF_slice')

mcmc <- buildMCMC(conf.mcmc)
cmcmc <- compileNimble(mcmc, project = cmodel, resetFunctions = TRUE)

## fit model
samp <- runMCMC(cmcmc, niter = 20000, nburnin = 10000, nchains=3, 
                inits = inits, progressBar = TRUE)
summ<-MCMCsummary(samp)

#saveRDS(samp, 'CJS_Model2v.rds')


##write out summary table
out<-cbind(rownames(summ), round(summ[,c(1,2,3,5)], dig=3))
#write_xlsx(out, 'CJS_Model2v.xlsx')
#MCMCtrace(samp)

### Turn into site-level estimates averaged over sex
outmat<-do.call(rbind, samp)

##males
lphimat<-outmat[, grep('a.phi', colnames(outmat))]
#phimat2<-outmat[,grep('mean.phi', colnames(outmat))]
#phi.int<-log(phimat2/(1-phimat2))
phimat<-apply(lphimat, 2, function(x) plogis(x))

##females
lphimatf<-outmat[, grep('a.phi', colnames(outmat))]+
  matrix(outmat[, grep('b.sex', colnames(outmat))], nrow=nrow(outmat), ncol=9)
phimatf<-apply(lphimatf, 2, function(x) plogis(x))

##using estimates proportion of females, get site level average

avg.phi<-plogis(matrix(outmat[,grep('psi', colnames(outmat))], nrow=nrow(outmat), ncol=9)*
                  lphimatf + 
                  (1-matrix(outmat[,grep('psi', colnames(outmat))], nrow=nrow(outmat), ncol=9))*
                  lphimat)

### turn into dataframe
keep<-c(1:7,9,20)
phi.df<-data.frame(Site=keep,
                   Mean=round(apply(phimat, 2, mean), dig=3),
                   Median=round(apply(phimat, 2, median), dig=3),
                   sd=round(apply(phimat, 2, sd), dig=3),
                   lower=round(apply(phimat, 2, function(x)quantile(x, p=0.05)), dig=3),
                   upper=round(apply(phimat, 2, function(x)quantile(x, p=0.95)), dig=3) )
phi.dff<-data.frame(Site=keep,
                    Mean=round(apply(phimatf, 2, mean), dig=3),
                    Median=round(apply(phimatf, 2, median), dig=3),
                    sd=round(apply(phimatf, 2, sd), dig=3),
                    lower=round(apply(phimatf, 2, function(x)quantile(x, p=0.05)), dig=3),
                    upper=round(apply(phimatf, 2, function(x)quantile(x, p=0.95)), dig=3) )

phi.dfa<-data.frame(Site=keep,
                    Mean=round(apply(avg.phi, 2, mean), dig=3),
                    Median=round(apply(avg.phi, 2, median), dig=3),
                    sd=round(apply(avg.phi, 2, sd), dig=3),
                    lower=round(apply(avg.phi, 2, function(x)quantile(x, p=0.05)), dig=3),
                    upper=round(apply(avg.phi, 2, function(x)quantile(x, p=0.95)), dig=3) )


phi.out<-rbind(phi.df, phi.dff,phi.dfa)
phi.out$Sex<-rep(c('Male', 'Female', 'Average'), each=9)

phi.out<-phi.out[order(c(keep,keep, keep)),]

##write out - necessary for PVA
write_xlsx(phi.out, 'Survival_CJS2v.xlsx')
