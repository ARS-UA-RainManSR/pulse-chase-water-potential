#set directories
setwd("/Users/wksmith/Documents/GitHub/pulse-chase-water-potential")
github_dir <- "/Users/wksmith/Documents/GitHub/pulse-chase-water-potential"
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggpubr)

#################################################################################################################
#open data files
data <- read.csv2(paste(github_dir,'/data/Data_Level1/',"Indices_Rep1_Merged.csv",sep=''),sep=',',header=T)
#Measurements
data$WP<-as.numeric(data$WP)
data$RWC<-as.numeric(data$RWC)
#Water Indices
data$NDWI1<-as.numeric(data$NDWI1)
data$NDWI2<-as.numeric(data$NDWI2)
data$NDWI3<-as.numeric(data$NDWI3)
data$WI1<-as.numeric(data$WI1)
data$WI2<-as.numeric(data$WI2)
data$WI3<-as.numeric(data$WI3)
#Chlorophyll indices
data$NDVI<-as.numeric(data$NDVI)
data$CI1<-as.numeric(data$CI1)
data$CI2<-as.numeric(data$CI2)
data$PRI<-as.numeric(data$PRI)


#PLOTS
####################Remote Sensing Comparison##########################
ggplot(data, aes(x=WI1, y=NDWI1,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Date)+
  geom_smooth(method=lm,linetype="dashed",color="black", fill="lightgrey")+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Scatterplot_WI1_NDWI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=CI1, y=CI2,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Date)+
  geom_smooth(method=lm,linetype="dashed",color="black", fill="lightgrey")+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Scatterplot_CI1_CI2.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=WI1, y=CI2,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Date)+
  geom_smooth(method=lm,linetype="dashed",color="black", fill="lightgrey")+
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste(github_dir,'/figures/',"Scatterplot_WI1_CI2.png",sep=""),dpi=300,width=180,height=120,units='mm')

####################Time Series Plots##########################
wp_mean_std <- data %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(WP), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

ggplot(wp_mean_std, aes(x=Date, y=mean,color=Time)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.05))+
  facet_wrap(~Summer)+
  geom_point(size=3)+
  labs(y='Water Potential')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Timeseries_WP.png",sep=""),dpi=300,width=180,height=120,units='mm')

rwc_mean_std <- data %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(RWC), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

ggplot(rwc_mean_std, aes(x=Date, y=mean,color=Time)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.05))+
  facet_wrap(~Summer)+
  geom_point(size=3)+
  labs(y='Relative Water Content')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Timeseries_RWC.png",sep=""),dpi=300,width=180,height=120,units='mm')

wi1_mean_std <- data %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(WI1), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

ggplot(wi1_mean_std, aes(x=Date, y=mean,color=Time)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.05))+
  facet_wrap(~Summer)+
  geom_point(size=3)+
  labs(y='WI')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Timeseries_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ndvi_mean_std <- data %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(CI1), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

ggplot(ndvi_mean_std, aes(x=Date, y=mean,color=Time)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.05))+
  facet_wrap(~Summer)+
  geom_point(size=3)+
  labs(y='NDVI')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Timeseries_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

pri_mean_std <- data %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(PRI), list(mean=mean, sd=sd)) %>% 
  as.data.frame()

ggplot(pri_mean_std, aes(x=Date, y=mean,color=Time)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.05))+
  facet_wrap(~Summer)+
  geom_point(size=3)+
  labs(y='PRI')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Timeseries_PRI_0814_0823.png",sep=""),dpi=300,width=180,height=120,units='mm')

####################Water Content Comparison##########################
ggplot(data, aes(x=RWC, y=WP,color=Time)) +
  geom_point(size=3)+
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(y='Water Potential',x='Relative Water Content')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 20))
ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_RWC.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=WI1, y=WP,color=Time)) +
  geom_point(size=3)+
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='Water Index',y='Water Potential')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=NDVI, y=WP,color=Time)) +
  geom_point(size=3)+
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='NDVI',y='Water Potential')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=WI1, y=RWC,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='Water Index',y='RWC')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=NDVI, y=RWC,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='CI',y='RWC')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=CI1, y=RWC,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='NDVI',y='RWC')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_CI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(data, aes(x=PRI, y=RWC,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='PRI',y='RWC')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_PRI.png",sep=""),dpi=300,width=180,height=120,units='mm')
