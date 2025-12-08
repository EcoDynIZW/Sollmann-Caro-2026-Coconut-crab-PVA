######################################################################
#### make plots from population projections ##########################

## set working directory to repository directory

library(ggplot2)
library(ggbreak)
library(RColorBrewer)
library(scales)
library(reshape2)
library(readxl)
library(writexl)
library(cowplot)
library(ggpubr)

## read in threat info
threats<-readRDS('data/threats_processed.rds')

##index for sites with specific estimates
keep<-c(1,2,3,4,5,6,7,9,20)

## function to calculate 2.5th and 97.5th percentile
qfun<-function(x)quantile(x, c(0.025, 0.975))


######################################################################
### first, plot of 9 good sites only #################################

## Supplement S1, Figure S3

out.df0<-readRDS('ProjectionsNineGoodSites.rds')

mean.df0<-aggregate(data=out.df0, N~site+T, FUN = 'median')

##calculate metapopulation (median and CIs)
nmeta<-aggregate(data=out.df0, N~T+iter, FUN=sum)
nmeta2<-aggregate(nmeta, N~T, FUN=median)
nmci<-aggregate(nmeta, N~T, FUN=qfun)
nmeta2$l95<-nmci$N[,1]
nmeta2$u95<-nmci$N[,2]

pp<-ggplot(data=mean.df0, aes(x=T, y=N, color=site))+
  geom_line(linewidth=0.8)+
  #scale_color_manual(breaks=names(colvec), values=colvec)+
  #scale_y_break(breaks = c(uw,lw), scales=0.2, space=0.1)+
  scale_y_break(breaks = c(3000, 4000), scales=0.2, space=0.1)+
  scale_x_continuous(breaks=seq(1,10,1))+
  geom_line(data=nmeta2, aes(x=T, y=N), color="black", linetype="dashed")+
  geom_ribbon(data=nmeta2, aes(ymin=l95, ymax=u95), 
              alpha=0.3, color='grey')+
  theme_bw()+
  theme(legend.position = 'right',
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank())+
  xlab('Year')+ylab('Population size')

pp

jpeg('Supp_FigS3.jpg', width=18, height=12, units = 'cm', res=300)
pp
dev.off()

######################################################################
#### plot of trajectories of entire metapopulation

## read in status quo results
out.df<-readRDS('ProjectionsStatusQuo.rds')

## some settings
nsites<-length(unique(out.df$site))
Tt<-10
niter=500


#######################################################################
### read in future scenarios 
scen<-c('certain', 'likely', 'possible')

out.df2<-readRDS('ProjectionsScenarios.rds')
out.df.all<-rbind(out.df, out.df2)

##average site level trajectories
mean.df2<-aggregate(data=out.df.all, N~site+T+Scenario, FUN = 'median')

##reorder factor levels
mean.df2$Scenario<-factor(mean.df2$Scenario, levels=c('status quo', scen))

##calculate metapopulation with CI
nmeta12<-aggregate(data=out.df.all, N~T+iter+Scenario, FUN=sum)
nmeta2<-aggregate(nmeta12, N~T+Scenario, FUN=median)
nmci<-aggregate(nmeta12, N~T+Scenario, FUN=qfun)

nmeta2<-cbind(nmeta2, nmci$N[,1], nmci$N[,2])
colnames(nmeta2)[4:5]<-c('l95', 'u95')
##reorder factor levels
nmeta2$Scenario<-factor(nmeta2$Scenario, levels=c('status quo', scen))

##make df to add status quo median line to all scenario panels
sq.df<-nmeta2[nmeta2$Scenario == 'status quo',]
sq.df<-sq.df[rep(1:nrow(sq.df), 3),]
sq.df$Scenario<-rep(scen, each = 10)


### Pieces for Figure 3

### plot metapopulations under 3 scnearios, with status qup trajectory in each panel
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
colv<-rep(gg_color_hue(3), each=10)

pp.alt<-ggplot(data=nmeta2[nmeta2$Scenario != 'status quo',], 
                 aes(x=T, y=N))+
  geom_line(linewidth=0.8, color=colv)+
  scale_x_continuous(breaks=seq(2,10,2))+
  geom_ribbon(aes(ymin=l95, ymax=u95), alpha=0.3, 
              color=NA, fill=colv)+
  geom_line(data=sq.df, aes(x=T, y=N), color='forestgreen',
            linetype='dashed', linewidth=0.8)+
  theme_bw()+
  theme(legend.position = 'none',
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank())+
  xlab('Year')+ylab('Metapopulation size')+
  facet_wrap(vars(Scenario), nrow=3, ncol=1)
pp.alt


#### make a multi plot figure with two more plots:
## total losses for each scenario and probability of decline

## read in results on numbers lost and 
## loss lists
ll<-readRDS('LossesScenarios.rds')
lost.list<-ll$lost.list #hotel
lost.list2<-ll$lost.list2 #housing+ag
exp.list<-ll$exp.list #exploitation
rm(ll)

##split loss by ag and housing according to proportion lost to each cause
pagloss<-threats$`Agric%` /(threats$`Agric%`+threats$`House%`)
pagloss[is.na(pagloss)]<-0

## create list of losses separately for ag and housing
lost.list2a<-lapply(lost.list2, function(x) {
  t(apply(x,1,function(y) y*pagloss))
})
##list housing
lost.list2b<-lapply(lost.list2, function(x) {
  t(apply(x,1,function(y) y*(1-pagloss)))
})

### functions to calculate losses

make.summary<-function(xx, group, prop=FALSE){
  
  out<-apply(xx[,group], 1, sum)
  
  ##if proportional loss, summarize nstart
  if(prop){
    n<-apply(nstart[,group], 1, sum)
    out<-out/n
  }
  return(out)
}


make.plot.df<-function(ll, lossname, grouping, grpname, prop=T){
  tt<-lapply(ll, function(x) make.summary(xx=x,grouping, prop=prop))
  tt2<-data.frame(value=unlist(tt),
                  variable =  grpname,
                  Scenario = rep(scen, each=niter),
                  Loss = lossname)
  return(tt2)
}

###calculate total loss for each cause
df.ht<-make.plot.df(ll=lost.list, 
                    lossname='Hotel',
                    grpname='all', 
                    grouping=rep(TRUE, nsites),
                    prop=F)
df.ot<-make.plot.df(ll=lost.list2, 
                    lossname='Housing+Ag',
                    grpname='all', 
                    grouping=rep(TRUE, nsites),
                    prop=F)
df.et<-make.plot.df(ll=exp.list, 
                    lossname='Offtake',
                    grpname='all', 
                    grouping=rep(TRUE, nsites),
                    prop=F)

##sum the three causes
df.all<-df.ht
df.all$value<-df.ht$value+df.ot$value+df.et$value

##make boxplot (Figure 2C)
p.loss<-ggplot(data=df.all, aes(x=Scenario, y=value, fill=Scenario))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = 'none')+
  ylab('Individuals lost')+xlab('')

### calculate probability of decline of metapopulation
  
###decline of metapop vs growth
gfun<-function(x)x[10]<x[1]

#nmeta12<-aggregate(data=out.df2, N~T+iter+Scenario, FUN=sum)
gdf<-aggregate(data=nmeta12, N~iter+Scenario, FUN=gfun)

gdf2<-aggregate(gdf, N~Scenario, FUN=sum)
gdf2$N<-gdf2$N/niter

## make barplot (Figure 2B)
p.dec<-ggplot(data=gdf2[gdf2$Scenario != 'status quo',],
              aes(x=Scenario, y=N, fill=Scenario))+
  geom_col()+
  theme_bw()+
  theme(legend.position = 'none')+
  xlab('')+
  ylab('Probability of decline')+
  geom_hline(yintercept = gdf2$N[gdf2$Scenario == 'status quo'],
             linewidth=0.8, color='forestgreen', 
             linetype='dashed')


#### combine plots into panel: trajectories left, other two right
test<-ggarrange(pp.alt,                                                 # First row with scatter plot
                ggarrange(p.dec, p.loss, nrow = 2, 
                          labels = c("B", "C"),
                          align='v'), # Second row with box and dot plots
                ncol = 2, 
                labels = "A"  ,
               widths = c(1, 0.8))                                      # Labels of the scatter plot

jpeg('MainFig3.jpg', width=16, height=14, units = 'cm', res=300)
test
dev.off()


#####################################################################
### losses by group and cause (Figure 4)

nstart<-matrix(threats$N, niter, nsites, byrow=T)
# 
is.island<-threats$Island=='Y'
is.pemba<-threats$Region=='Pemba'

##set the 4 geographic groups
group.list<-list(IP= (is.island & is.pemba),
                 IU=(is.island & !is.pemba),
                 MP =(!is.island & is.pemba),
                 MU =(!is.island & !is.pemba))

##calculate absolute expected loss per cause and scenario
##set prop=F for absolute loss
df.hotel<-lapply(1:length(group.list), 
                 function(x) make.plot.df(ll=lost.list, 
                                          lossname='Hotel',
                                          grpname=names(group.list)[x], 
                                          grouping=group.list[[x]],
                                          prop=F))

pdf1<-do.call(rbind, df.hotel)#hotel

df.othera<-lapply(1:length(group.list), 
                  function(x) make.plot.df(ll=lost.list2a, 
                                           lossname='Ag',
                                           grpname=names(group.list)[x], 
                                           grouping=group.list[[x]],
                                           prop=F))
pdf2a<-do.call(rbind, df.othera) #other

df.otherb<-lapply(1:length(group.list), 
                  function(x) make.plot.df(ll=lost.list2b, 
                                           lossname='Housing',
                                           grpname=names(group.list)[x], 
                                           grouping=group.list[[x]],
                                           prop=F))
pdf2b<-do.call(rbind, df.otherb) #other

df.exploit<-lapply(1:length(group.list), 
                   function(x) make.plot.df(ll=exp.list, 
                                            lossname='Offtake',
                                            grpname=names(group.list)[x], 
                                            grouping=group.list[[x]],
                                            prop=F))
pdf3<-do.call(rbind, df.exploit) #exploit

## combine into single data frame
pdf.full<-rbind(pdf1, pdf2a,pdf2b, pdf3)
pdf.full$Iteration<-rep(1:niter, nrow(pdf.full)/niter)

##calculate median loss
pdf.m<-aggregate(data=pdf.full, value~variable+Scenario+Loss, FUN=median)

### same with proportional loss
df.hotel<-lapply(1:length(group.list), 
                 function(x) make.plot.df(ll=lost.list, 
                                          lossname='Hotel',
                                          grpname=names(group.list)[x], 
                                          grouping=group.list[[x]],
                                          prop=T))

pdf1<-do.call(rbind, df.hotel)#hotel

df.othera<-lapply(1:length(group.list), 
                  function(x) make.plot.df(ll=lost.list2a, 
                                           lossname='Ag',
                                           grpname=names(group.list)[x], 
                                           grouping=group.list[[x]],
                                           prop=T))
pdf2a<-do.call(rbind, df.othera) #other

df.otherb<-lapply(1:length(group.list), 
                  function(x) make.plot.df(ll=lost.list2b, 
                                           lossname='Housing',
                                           grpname=names(group.list)[x], 
                                           grouping=group.list[[x]],
                                           prop=T))
pdf2b<-do.call(rbind, df.otherb) #other

df.exploit<-lapply(1:length(group.list), 
                   function(x) make.plot.df(ll=exp.list, 
                                            lossname='Offtake',
                                            grpname=names(group.list)[x], 
                                            grouping=group.list[[x]],
                                            prop=T))
pdf3<-do.call(rbind, df.exploit) #exploit

pdf.fullp<-rbind(pdf1, pdf2a, pdf2b, pdf3)
pdf.fullp$Iteration<-rep(1:niter, nrow(pdf.fullp)/niter)

##calculate median loss
pdf.mp<-aggregate(data=pdf.fullp, value~variable+Scenario+Loss, FUN=median)

##set facet lables
supp.labs <- c("Islets Pemba","Islets Unguja", "Mainland Pemba",
               "Mainland Unguja")
names(supp.labs) <- c("IP", "IU", 'MP', 'MU')

##plot absolute and proportional losses
pl<-ggplot(data=pdf.m, aes(x=Scenario, y=value, fill=Loss))+
  geom_bar(position='stack', stat='identity')+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        axis.text.x=element_blank(),
       axis.ticks.x = element_blank() )+
  ylab('Individuals lost')+
  facet_grid(~variable, labeller = labeller(variable=supp.labs))

pl2<-ggplot(data=pdf.mp, aes(x=Scenario, y=value, fill=Loss))+
  geom_bar(position='stack', stat='identity')+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45,hjust=1))+
  ylab('Proportion lost')+
  facet_grid(~variable, labeller = labeller(variable=supp.labs))

## combine with single legend
pcomb<-ggarrange(pl, pl2, # Second row with box and dot plots
                nrow = 2, 
                labels = c("A", "B")  ,
                legend='bottom',
                align='v',
                common.legend = T,
                heights=c(0.8, 1)) 

jpeg('MainFig4.jpg', width=16, height=12, units = 'cm', res=300)
pcomb
dev.off()


######################################################################
#### make trajectories of individual subpopulations ##################

### Supplement S1, Figures S4-S6

## maybe by categories mainland-island, Pemba-Unguja
mean.df2$Category<-'MP'
idx<-pmatch(mean.df2$site, threats$Name, duplicates.ok = TRUE)
mean.df2$Category[threats$Region[idx] == 'Pemba' &
                    threats$Island[idx] == 'Y']<-'IP'

mean.df2$Category[threats$Region[idx] == 'Unguja' &
                    threats$Island[idx] == 'N']<-'MU'

mean.df2$Category[threats$Region[idx] == 'Unguja' &
                    threats$Island[idx] == 'Y']<-'IU'

##Pemba islets, Pemba mainland, Unguja
## select lines of code as appropriate below

# pp<-ggplot(data=mean.df2[threats$Region[idx] == 'Pemba' &
#                            threats$Island[idx] == 'N' ,], 
#            aes(x=T, y=N, color = Scenario))+
# pp<-ggplot(data=mean.df2[threats$Region[idx] == 'Pemba' &
#                              threats$Island[idx] == 'Y' ,], 
#              aes(x=T, y=N, color = Scenario))+
pp<-ggplot(data=mean.df2[threats$Region[idx] == 'Unguja' ,], 
             aes(x=T, y=N, color = Scenario))+
  geom_line(linewidth=0.8)+
  scale_x_continuous(breaks=seq(1,10,1))+
  theme_bw()+
  theme(#legend.position = 'none',
        axis.ticks.x.top = element_blank(),
        axis.text.x.top = element_blank(),
        legend.position = c(1, 0),
        legend.justification = c(1, 0))+
  xlab('Year')+ylab('Population size')+
  facet_wrap(vars(site), scales='free', ncol=4)
pp
jpeg('SuppFig6_U.jpg', width=20, height=29, units='cm', res=300)
pp
dev.off()
######################################################################
