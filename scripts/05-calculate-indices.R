# Process raw hyperspec data into level 1
library(asdreader)
library(tidyverse)

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
#Calculates difference ratio using (a:b - c:d) / (c:d)
calc_diffR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  ((rowMeans(df_wide[b1_1:b1_2]) - rowMeans(df_wide[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df_wide[b2_1:b2_2])))
}
#Calculates additive ratio using (a:b + c:d) / (c:d + e:f)
calc_addR <- function(df, a, b, c, d, e, f){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  b3_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b3_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  ((rowMeans(df_wide[b1_1:b1_2]) + rowMeans(df_wide[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df_wide[b2_1:b2_2]) + rowMeans(df_wide[b3_1:b3_2])))
}
#Calculates complex using (a:b) / (c:d + e:f + g:h + i:j)
calc_complex <- function(df, a, b, c, d, e, f, g, h, i, j){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  b3_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b3_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b4_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fourth band and end range
  b4_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fourth band and end range
  b5_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fifth band and end range
  b5_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fifth band and end range
  ((rowMeans(df_wide[b1_1:b1_2])) /      #vegetation index equation
      (rowMeans(df_wide[b2_1:b2_2]) + rowMeans(df_wide[b3_1:b3_2]) + 
         rowMeans(df_wide[b4_1:b4_2]) + rowMeans(df_wide[b5_1:b5_2])))
}
################################################################################
#Location of all the raw .sig files
folders <- c('08_14','08_15','08_16','08_18','08_21','08_22','08_23','08_26','08_30','09_04')
dates <- c('8/14/2023','8/15/2023','8/16/2023','8/18/2023',
           '8/21/2023','8/22/2023','8/23/2023','8/26/2023',
           '8/30/2023','9/4/2023')
to_merge <-c()
master <- read_csv("data/RainMan_Pulse23_Spectra_090723.csv")
master$REP1 <- str_pad(master$REP1, 5, pad = "0")
master$REP2 <- str_pad(master$REP2, 5, pad = "0")

# loop to read, calculate, and combine
for(i in 1:length(dates)){
  print(i)
  daily_master <- master[which(master$DATE == dates[i]), ]
  file_list <- c()
  time_id <- c()
  house_id <- c()
  plot_id <- c()
  treat_id <- c()
  winter_id <- c()
  summer_id <- c()
  rep_id <- c()
  full_id <- c()
  scan_id <- c()
  date_id <- c()
  wp_id <- c()
  rwc_id <- c()
  
  for(j in 1:length(daily_master$REP1)){
    file_list[j] <- list.files(paste0("data/hyperspec_0/", folders[i], '/'), 
                               full.names = T, 
                               pattern = paste0(daily_master$REP1[j], ".asd"))
    scan_id[j] <- sub(".*/", "", file_list[j])
    date_id[j] <- daily_master$DATE[j]
    time_id[j] <- daily_master$TIME[j]
    house_id[j] <- paste0("H", daily_master$HOUSE[j])
    plot_id[j] <- paste0("P", daily_master$PLOT[j])
    treat_id[j] <- daily_master$TREAT[j]
    winter_id[j] <- substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[1]
    summer_id[j] <- substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[2]
    rep_id[j] <- daily_master$DUP[j]
    full_id[j] <- paste0(house_id[j], plot_id[j], treat_id[j], time_id[j], rep_id[j])
    wp_id[j] <- daily_master$WP[j]
    rwc_id[j] <- daily_master$RWC[j]
  }
  
  #For loop to read and append all scans to single df
  dataset <- data.frame()
  for (j in 1:length(file_list)){
    md <- get_metadata(file_list[j])
    #read in hyperspectral data using 'asdreader' package
    temp_data <- asdreader::get_spectra(file_list[j]) 
    data <- as.numeric(temp_data)
    wvl <- as.integer(colnames(temp_data))
    
    #########Spectra Processing following Schweiger et al. Nature Ecology and Evolution (2018)################
    #breakpoint #1 that seperates spectrometer 1 from spectrometer 2 (correction for any discontinuinty)
    c1 <- data[which(wvl == 1000)] - data[which(wvl == 1001)] 
    
    #breakpoint #2 that seperates spectrometer 2 from spectrometer 3 (correction for any discontinuinty)
    c2 <- data[which(wvl == 1800)] - data[which(wvl == 1801)] 
    
    # combine and corrected for raw reflectance data
    data_c <- c(data[1:which(wvl == 1000)], data[which(wvl == 1001):which(wvl == 1800)] + c1,
                data[which(wvl == 1801):which(wvl == 2500)] + c1 + c2)
    
    #data standardization step (see Schweiger et al. 2018)
    data_s <- data_c/sqrt(sum(data_c^2)) 
    
    date <- rep(date_id[j], length(wvl))
    time <- rep(time_id[j], length(wvl))
    house <- rep(house_id[j], length(wvl))
    plot <- rep(plot_id[j], length(wvl))
    treat <- rep(treat_id[j], length(wvl))
    winter <- rep(winter_id[j], length(wvl))
    summer <- rep(summer_id[j], length(wvl))
    full <- rep(full_id[j], length(wvl))
    scan <- rep(scan_id[j], length(wvl))
    
    ds <- cbind(date, wvl, data_s, time, 
                house, plot, treat, 
                winter, summer, full, scan)
    dataset <- rbind(dataset, ds)
  }
  #Name the columns
  names(dataset) <- c("date", "wavelength", "reflectance", "time", 
                      "house", "plot", "treat", "winter", "summer",
                      "full", "scan")
  dataset <- transform(dataset, wavelength = as.numeric(wavelength))
  dataset <- transform(dataset, reflectance = as.numeric(reflectance))
  
  #####Reflectance Plots#################
  ggplot(dataset, 
         aes(x = wavelength, y = reflectance,
             group = full, color = time)) +
    geom_line(show.legend = T, linewidth =.8, linetype = "solid") +
    facet_wrap(~summer) +
    scale_y_continuous("Reflectance") +
    scale_x_continuous("Wavelength (nm)", 
                       limits = c(400, 2400), 
                       breaks = seq(400, 2400, 400)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    theme(text = element_text(size = 40)) +
    theme_bw()
  ggsave(paste0('figures/Hyperspectral_Reflectance_Rep1_',folders[i],'.png'),
         dpi = 300,
         width = 180,
         height = 120,
         units = 'mm')
  
  ####################################################################################################
  #VIs - Convert to wide format
  df_wide <- dataset %>% select(wavelength, reflectance, full) %>%
    pivot_wider(names_from = wavelength, 
                values_from = reflectance, 
                id_cols = full, 
                values_fn = mean)
  
  #NDVI
  df_wide$NDVI <- calc_VI(df_wide, 850, 850, 650, 650) #literature derived chlorophyll proxies
  df_wide$CI1 <- calc_VI(df_wide, 750, 750, 550, 550) # chlorophyll index 1
  df_wide$CI2 <- calc_VI(df_wide, 750, 750, 710, 710) # chlorophyll index 2
  
  #NDWI
  df_wide$WI1 <- calc_SR(df_wide, 900, 900, 970, 970) #literature derived water content proxies
  df_wide$WI2 <- calc_SR(df_wide, 1600, 1600, 820, 820)
  df_wide$WI3 <- calc_SR(df_wide, 860, 860, 1240, 1240)
  df_wide$NDWI1 <- calc_VI(df_wide, 860, 860, 1240, 1240)
  df_wide$NDWI2 <- calc_VI(df_wide, 860, 860, 1640, 1640)
  df_wide$NDWI3 <- calc_VI(df_wide, 860, 860, 2130, 2130)
  
  #PRI
  # Penuelas et al. 1995
  df_wide$PRI <- calc_VI(df_wide, 531, 531, 570, 570) #literature derived PRI bands
  
  # From Caine et al. 2024 PCE
  
  # WBI - water band index
  # Penuelas et al. 1993, R970/R900
  df_wide$WBI <- calc_SR(df_wide, 970, 970, 900, 900)
  
  # WP1 - water potential index 1
  # Mertens et al. 2021, (R665 - R715)/R715
  df_wide$WPI1 <- calc_diffR(df_wide, 660, 670, 710, 720)
  
  # WP2 - water potential index 2
  # Mertens et al. 2021, (R665 + R1457)/(R715 + R1457)
  df_wide$WPI2 <- calc_addR(df_wide, 660, 670, 1457, 1457, 710, 720)
  
  # RWC - relative water content index
  # Yu et al. 2000, R1430/R1850
  df_wide$RWC_ind <- calc_SR(df_wide, 1430, 1430, 1850, 1850)
  
  # CNDI - combined nitrogen and drought index
  # Cainet al. 2024, R1353/(R706 + R1402 + R1451 + R1878)
  df_wide$CNDI <- calc_complex(df_wide, 1353, 1353, 706, 706, 1402, 1402, 1451, 1451, 1878, 1878)
  
  #write csv file
  out <- cbind.data.frame(date_id,
                          df_wide$full,
                          house_id, plot_id, treat_id, 
                          winter_id, summer_id, time_id,
                          df_wide$NDVI, df_wide$CI1, df_wide$CI2, 
                          df_wide$NDWI1, df_wide$NDWI2, df_wide$NDWI3, 
                          df_wide$WI1, df_wide$WI2, df_wide$WI3, 
                          df_wide$PRI, df_wide$WBI, # from Penuelas et al. 1993, 1995
                          df_wide$WPI1, df_wide$WPI2, # from Mertens et al. 2021
                          df_wide$RWC_ind, # from Yu et al. 2000
                          df_wide$CNDI, # from Caine et al. 2024
                          wp_id, rwc_id)
  
  colnames(out) <- c('Date', 'ID', 'House', 'Plot', 'Treat', 'Winter', 'Summer', 'Time',
                   'NDVI', 'CI1', 'CI2', 'NDWI1', 'NDWI2', 'NDWI3', 'WI1', 'WI2', 'WI3',
                   'PRI', 'WBI',
                   'WPI1', 'WPI2',
                   'RWC_ind',
                   'CNDI',
                   'WP', 'RWC')
  
  write_csv(out, paste0('data/hyperspec_1/Indices_Rep1_',folders[i],".csv"))
  print(dates[i])
  print(dim(out))
}


##### Merge files, remove duplicates, and rename ####

fn <- list.files("data/hyperspec_1/")

to_merge <- list()
for(i in 1:length(fn)){
  temp <- read_csv(paste0("data/hyperspec_1/", fn[i]),
                   locale = locale(tz = "America/Phoenix")) 
  to_merge[[i]] <- temp
}

merged <- do.call(rbind, to_merge) |> 
  rename(date_col = Date,
         ID_long = ID,
         house = House, plot = Plot, trt = Treat,
         trt_w = Winter, trt_s = Summer, period = Time) |> 
  mutate(ID = paste0(house, plot),
         house = str_extract(house, "[0-9]+"),
         plot = str_extract(plot, "[0-9]+")) |> 
  select(-WP, -RWC)
colnames(merged)
# find duplicates
dups <- merged |> 
  group_by(date_col, period, trt_s, ID) |> 
  count() |> 
  filter(n > 1) |> 
  select(-n) |> 
  inner_join(merged) |> 
  slice(1,3) # keep the first of each measurement, which are removed in the next step

# remove duplicates
out <- merged |> 
  anti_join(dups)

write_csv(out, paste0('data_clean/hyp_indices.csv'))

