
##set working directory to repository directory

library(reshape2)
library(ggplot2)
library(scales)
library(ggpubr)

threats<-readRDS('data/threats_processed.rds')

##define the 4 geographic regions
threats$Type<-'IP'
threats$Type[threats$Island == 'N'&
               threats$Region == 'Pemba']<-'MP'
threats$Type[threats$Island == 'N'&
               threats$Region == 'Unguja']<-'MU'
threats$Type[threats$Island == 'Y'&
               threats$Region == 'Unguja']<-'IU'

pfun<-function(x) sum(x>0)
##extant crab habitats in each region
habtab<-aggregate(threats,Hab.Area.ha~Type, FUN=sum)
##number of subpopulations in each region
poptab<-aggregate(threats, N~Type, FUN=pfun)

scen<-c('certain', 'likely', 'possible')
indc<-c('y', 'y?', 'n?')

cause<-c("Hotel", "agriculture", "housing", "exploitation")
cause.p<-c("Hotel%", "Agric%", "House%", NA)

##tabulate amount of habitat lost, individuals affected by each threat
## under each scenario
nsites<-nrow(threats)
for (s in 1: length(scen)){
  for (i in 1:length(cause)){
    bb<-rep(0, nsites)
    bb[which(threats[,cause[i]] %in% indc[1:s])]<-1
    
    nam<-paste(cause[i], scen[s], sep='_')
    nam2<-paste(cause[i], scen[s], 'N',sep='_')
    if(i<4){
    threats[,nam]<-threats$Hab.Area.ha*bb*(threats[,cause.p[i]]/100)
    }
    threats[,nam2]<-threats$N*bb#*(threats[,cause.p[i]]/100)
  }}

##create formula to summarize by region
allb<-colnames(threats)[19:39]
by1<-allb[!grepl('N', allb)]
by2<-allb[grepl('N', allb)]

x<-formula(paste("cbind(", paste(by1, collapse = ','), ") ~ ",'Type'))
x2<-formula(paste("cbind(", paste(by2, collapse = ','), ") ~ ",'Type'))

##habitat lost
hab.df<-aggregate(data=threats, x, FUN=sum)
##subpopulations affected
pop.df<-aggregate(data=threats, x2, FUN=pfun)

plot.df<-rbind(melt(hab.df), melt(pop.df))

##create threat and scenario columns
plot.df$Threat<-sapply(strsplit(as.character(plot.df$variable), '_'), function(xx) xx[1])
plot.df$Certainty<-sapply(strsplit(as.character(plot.df$variable), '_'), function(xx) xx[2])

##add variable: Habitat or N
isn<-grepl('N', plot.df$variable)
plot.df$V<-'habitat'
plot.df$V[isn]<-'N'

##calculate proportional habitat loss/subpopulations affected
hab.df2<-cbind(hab.df, pop.df[,-1])
mm<-cbind(matrix(habtab$Hab.Area.ha, nrow=4, ncol=9),
          matrix(poptab$N, nrow=4, ncol=12))
hab.df2[,2:ncol(hab.df2)]<-round(hab.df2[,2:ncol(hab.df2)]/mm, dig=3)



plot.d2<-melt(hab.df2)

plot.df$Prop<-plot.d2$value
#reorder, rename factor levels
plot.df$Threat<-factor(plot.df$Threat, 
                       levels=c('Hotel', 'agriculture', 'housing', 'exploitation'))
levels(plot.df$Threat)<-c('Hotel', 'Ag', 'Housing', 'Offtake')

##change facet labels
supp.labs <- paste0(c("Islets Pemba","Islets Unguja", "Mainland Pemba",
                      "Mainland Unguja"),'\n(', round(habtab$Hab.Area.ha), ' ha)')
names(supp.labs) <- c("IP", "IU", 'MP', 'MU')

##make sure colors are consistent
colvec<-hue_pal()(4) [1:3] #c("#F8766D",  "#7CAE00","#00BFC4")#, "#C77CFF")
names(colvec)<-c('Hotel', 'Ag', 'Housing')#, 'Offtake')

##plot for habitat lost
p2<-ggplot(plot.df[plot.df$V =='habitat',], aes(x=Certainty, y=Prop, fill=Threat))+
  geom_bar(position='stack', stat='identity')+
  scale_fill_manual(values=colvec)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        # axis.text.x = element_text(angle = 45,hjust=1),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank() )+
  ylab('Habitat at risk')+
  #geom_text(data=text.df, aes(x=x, y=y, label=label))+
  facet_grid(~Type, labeller = labeller(Type=supp.labs))
p2


##plot for subpopulations affected
supp.labs2 <- paste0(c("Islets Pemba","Islets Unguja", "Mainland Pemba",
               "Mainland Unguja"),' (', poptab$N, ')')
names(supp.labs2) <- c("IP", "IU", 'MP', 'MU')

p3<-ggplot(plot.df[plot.df$V =='N',], aes(x=Certainty, y=Prop, fill=Threat))+
  geom_bar(position='dodge', stat='identity')+
  scale_y_continuous(breaks=c(0, 0.5, 1))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 45,hjust=1),
        #axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.y = element_blank())+
  ylab('Subpopulations at risk')+
  facet_grid(rows=vars(Threat), cols=vars(Type), labeller = labeller(Type=supp.labs2))
p3



###combine

test<-ggarrange(p2, # First row 
                p3, # Second row 
                nrow = 2, 
                #align = 'h',
                labels = c("A", "B")  ,
                heights = c(0.35, 1))                                      # Labels of the scatter plot

jpeg('Figures/ThreatsBoth.jpg', width=15, height=20, 
     units = 'cm', res=300)
test
dev.off()
