#set directories
# setwd("/Users/wksmith/Documents/GitHub/pulse-chase-water-potential")
# github_dir <- "/Users/wksmith/Documents/GitHub/pulse-chase-water-potential"
library(tidyverse)
library(ggpubr)

#################################################################################################################
#open data files
hyp_ind <- read_csv("data/hyperspec_1/Indices_Rep1_merged.csv") |> 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))
str(hyp_ind)

#NDVI, literature derived chlorophyll proxies
# NDVI, CI1, CI2

#NDWI, literature derived water content proxies
# WI1, WI2, WI3, NDWI1, NDWI2, NDWI3

#PRI, literature derived photochemical reflectance index
# PRI


#PLOTS
####################Remote Sensing Comparison##########################
ggplot(hyp_ind, aes(x = WI1, y = NDWI1, color = Time)) +
  geom_point(size = 3) +
  facet_wrap(~Date) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", fill = "lightgrey") +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WI1_NDWI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x = CI1, y = CI2, color = Time)) +
  geom_point(size = 3) +
  facet_wrap(~Date) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", fill = "lightgrey") +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
# ggsave(paste(github_dir,'/figures/',"Scatterplot_CI1_CI2.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x = WI1, y = CI2, color = Time)) +
  geom_point(size = 3) +
  facet_wrap(~Date) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", fill = "lightgrey") +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WI1_CI2.png",sep=""),dpi=300,width=180,height=120,units='mm')

####################Time Series Plots##########################
wi1_mean_std <- hyp_ind %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(WI1), list(mean = mean, sd = sd))

ggplot(wi1_mean_std, aes(x = Date, y = mean,color = Time)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                width = 0.2, position = position_dodge(0.15)) +
  facet_wrap(~Summer) +
  geom_point(size = 3) +
  scale_x_date(date_labels = "%m-%d") +
  labs(y = 'WI') +
  theme_bw(base_size = 14)
# ggsave(paste(github_dir,'/figures/',"Timeseries_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ndvi_mean_std <- hyp_ind %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(CI1), list(mean=mean, sd=sd))

ggplot(ndvi_mean_std, aes(x = Date, y = mean,color = Time)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                width = 0.2, position = position_dodge(0.15)) +
  facet_wrap(~Summer) +
  geom_point(size = 3) +
  scale_x_date(date_labels = "%m-%d") +
  labs(y = 'NDVI') +
  theme_bw(base_size = 14)
# ggsave(paste(github_dir,'/figures/',"Timeseries_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

pri_mean_std <- hyp_ind %>%
  group_by(Summer, Time, Date) %>% 
  summarise_at(vars(PRI), list(mean = mean, sd = sd))

ggplot(pri_mean_std, aes(x = Date, y = mean,color = Time)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), 
                width = 0.2, position = position_dodge(0.15)) +
  facet_wrap(~Summer) +
  geom_point(size = 3) +
  scale_x_date(date_labels = "%m-%d") +
  labs(y = 'PRI') +
  theme_bw(base_size = 14)
# ggsave(paste(github_dir,'/figures/',"Timeseries_PRI_0814_0823.png",sep=""),dpi=300,width=180,height=120,units='mm')

####################Water Content Comparison##########################
ggplot(hyp_ind, aes(x=RWC, y=WP,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_RWC.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=WI1, y=WP,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=NDVI, y=WP,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WP_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=WI1, y=RWC,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_WI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=NDVI, y=RWC,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=NDVI, y=WI1,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='NDVI',y='WI')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
# ggsave(paste(github_dir,'/figures/',"Scatterplot_WI1_NDVI.png",sep=""),dpi=300,width=180,height=120,units='mm')


ggplot(hyp_ind, aes(x=CI1, y=WP,color=Time)) +
  geom_point(size=3)+
  facet_wrap(~Summer)+
  geom_smooth(method=lm,aes(group = Time))+
  stat_cor(aes(group = Time,label = after_stat(rr.label)),geom = "label",label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  #geom_smooth(method=lm,formula=y ~ poly(x, 2, raw=TRUE),color='black',fill=NA)+
  #stat_cor(aes(label = after_stat(rr.label)),geom = "label",color='black',label.y.npc="bottom", label.x.npc = "right",hjust=1)+
  labs(x='NDVI',y='WP')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(text = element_text(size = 10))
# ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_CI1.png",sep=""),dpi=300,width=180,height=120,units='mm')

ggplot(hyp_ind, aes(x=PRI, y=RWC,color=Time)) +
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
# ggsave(paste(github_dir,'/figures/',"Scatterplot_RWC_PRI.png",sep=""),dpi=300,width=180,height=120,units='mm')
