setwd("/Users/wksmith/Documents/GitHub/pulse-chase-water-potential")
github_dir <- "/Users/wksmith/Documents/GitHub/pulse-chase-water-potential"
library(asdreader)
library(reshape2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)

###FUNCTIONS##################################################################################
#Calculates vegetation simple ratio (a:b) / (c:d)
calc_SR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  (rowMeans(df_wide[b1_1:b1_2])/(rowMeans(df_wide[b2_1:b2_2])))
}
#Calculates normalized index using (a:b - c:d) / (a:b + c:d)
calc_VI <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  ((rowMeans(df_wide[b1_1:b1_2]) - rowMeans(df_wide[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df_wide[b1_1:b1_2]) + rowMeans(df_wide[b2_1:b2_2])))
}
################################################################################
#Location of all the raw .sig files
folders=c('08_14','08_15','08_16','08_18','08_21','08_22','08_23','08_26','08_30','09_04')
dates=c('8/14/23','8/15/23','8/16/23','8/18/23','8/21/23','8/22/23','8/23/23','8/26/23','8/30/23','9/4/23')
merge=c()
master <- read.csv2(paste(github_dir,'/data/Datasheet/RainMan_Pulse23_Spectra_090723.csv',sep=''),sep=',',header=T)
master$REP1<-str_pad(master$REP1, 5, pad = "0")
master$REP2<-str_pad(master$REP2, 5, pad = "0")
for(i in 1:length(dates)){
  print(i)
  daily_master=master[which(master$DATE==dates[i]),]
  file_list=c()
  time_id=c()
  house_id=c()
  plot_id=c()
  treat_id=c()
  winter_id=c()
  summer_id=c()
  full_id=c()
  scan_id=c()
  date_id=c()
  wp_id=c()
  rwc_id=c()
  
  for(j in 1:length(daily_master$REP1)){
    file_list[j] <- list.files(paste(github_dir,'/data/Data_Level0/',folders[i],'/',sep=''), full.names = T, pattern = paste(daily_master$REP1[j],".asd",sep=''))
    scan_id[j] <- sub(".*/", "", file_list[j])
    date_id[j]=daily_master$DATE[j]
    time_id[j]=daily_master$TIME[j]
    house_id[j]=paste("H",daily_master$HOUSE[j],sep='')
    plot_id[j]=paste("P",daily_master$PLOT[j],sep='')
    treat_id[j]=daily_master$TREAT[j]
    winter_id[j]=substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[1]
    summer_id[j]=substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[2]
    full_id[j]=paste(house_id[j],plot_id[j],treat_id[j],time_id[j],sep='')
    wp_id[j]=daily_master$WP[j]
    rwc_id[j]=daily_master$RWC[j]
  }
  
  #For loop to read and append all scans to single df
  dataset <- data.frame()
  for (j in 1:length(file_list)){
    md<-get_metadata(file_list[j])
    temp_data<-get_spectra(file_list[j])
    data<-as.numeric(temp_data)
    wvl=as.integer(colnames(temp_data))
    c1=data[which(wvl==1000)]-data[which(wvl==1001)]
    c2=data[which(wvl==1800)]-data[which(wvl==1801)]
    data_c=c(data[1:which(wvl==1000)],data[which(wvl==1001):which(wvl==1800)]+c1,data[which(wvl==1801):which(wvl==2500)]+c1+c2)
    data_s=data_c/sqrt(sum(data_c^2))
    date=rep(date_id[j],length(wvl))
    time=rep(time_id[j],length(wvl))
    house=rep(house_id[j],length(wvl))
    plot=rep(plot_id[j],length(wvl))
    treat=rep(treat_id[j],length(wvl))
    winter=rep(winter_id[j],length(wvl))
    summer=rep(summer_id[j],length(wvl))
    full=rep(full_id[j],length(wvl))
    scan=rep(scan_id[j],length(wvl))
    
    ds=cbind(date,wvl,data_s,time,house,plot,treat,winter,summer,full,scan)
    dataset <- rbind(dataset,ds)
  }
  #Name the columns
  names(dataset) <- c("date","wavelength", "reflectance","time","house","plot","treat","winter","summer","full","scan")
  dataset=transform(dataset,wavelength = as.numeric(wavelength))
  dataset=transform(dataset,reflectance = as.numeric(reflectance))
  
  #####Reflectance Plots#################
  ggplot(dataset, aes(x=wavelength,y=reflectance,group=full,color=time)) +
    geom_line(show.legend = T,linewidth=.8,linetype="solid") +
    facet_wrap(~summer)+
    scale_y_continuous("Reflectance") +
    scale_x_continuous("Wavelength (nm)",limits = c(400,2400), breaks = seq(400,2400,200)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    theme(text = element_text(size = 40)) +
    theme_bw()
  ggsave(paste(github_dir,'/figures/Hyperspectral_Reflectance_Rep1_',folders[i],'.png',sep=''),dpi=300,width=180,height=120,units='mm')
  
  ####################################################################################################
  #VIs - Convert to wide format
  df_wide <- dataset %>% select(wavelength, reflectance, full) %>%
    pivot_wider(names_from = wavelength, values_from = reflectance, id_cols = full, values_fn = mean)
  
  #NDVI
  df_wide$NDVI <- calc_VI(df_wide, 850, 850, 650, 650)
  df_wide$CI1 <- calc_VI(df_wide, 750, 750, 550, 550)
  df_wide$CI2 <- calc_VI(df_wide, 750, 750, 710, 710)
  
  #NDWI
  df_wide$WI1 <- calc_SR(df_wide, 900, 900, 970, 970)
  df_wide$WI2 <- calc_SR(df_wide, 1600, 1600, 820, 820)
  df_wide$WI3 <- calc_SR(df_wide, 860, 860, 1240, 1240)
  df_wide$NDWI1 <- calc_VI(df_wide, 860, 860, 1240, 1240)
  df_wide$NDWI2 <- calc_VI(df_wide, 860, 860, 1640, 1640)
  df_wide$NDWI3 <- calc_VI(df_wide, 860, 860, 2130, 2130)
  
  #PRI
  df_wide$PRI <- calc_VI(df_wide, 530, 530, 570, 570) 
  
  #write csv file
  out<-cbind(date_id,df_wide$full,house_id,plot_id,treat_id,winter_id,summer_id,time_id,
             df_wide$NDVI,df_wide$CI1,df_wide$CI2,df_wide$NDWI1,df_wide$NDWI2,df_wide$NDWI3,df_wide$WI1,df_wide$WI2,df_wide$WI3,df_wide$PRI,wp_id,rwc_id)
  colnames(out)<-c("Date",'ID','House','Plot','Treat','Winter','Summer','Time',
                   'NDVI','CI1','CI2','NDWI1','NDWI2','NDWI3','WI1','WI2','WI3','PRI','WP','RWC')
  write.csv(out,paste(github_dir,'/data/Data_Level1/Indices_Rep1_',folders[i],".csv",sep=''),row.names=FALSE,col.names=TRUE)
  print(dates[i])
  print(dim(out))
  merge<-rbind(merge,out)
}
colnames(out)<-c("Date",'ID','House','Plot','Treat','Winter','Summer','Time',
                 'NDVI','CI1','CI2','NDWI1','NDWI2','NDWI3','WI1','WI2','WI3','PRI','WP','RWC')
write.csv(merge,paste(github_dir,'/data/Data_Level1/Indices_Rep1_Merged.csv',sep=''),row.names=FALSE,col.names=TRUE)

