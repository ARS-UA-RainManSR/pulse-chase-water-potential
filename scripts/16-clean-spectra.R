# Plot full spectra by treatment
library(asdreader)
library(tidyverse)
# process full spectra

#Location of all the raw .sig files
folders <- c('08_14','08_15','08_16','08_18','08_21','08_22','08_23','08_26','08_30','09_04')
dates <- c('8/14/2023','8/15/2023','8/16/2023','8/18/2023',
           '8/21/2023','8/22/2023','8/23/2023','8/26/2023',
           '8/30/2023','9/4/2023')
to_merge <-c()
master <- read_csv("data/RainMan_Pulse23_Spectra_090723.csv")
master$REP1 <- str_pad(master$REP1, 5, pad = "0")
master$REP2 <- str_pad(master$REP2, 5, pad = "0")
# missing REP2 - 00067 is not available, and 00066 is empty
ind <- which(master$REP2 == "00067")
master$REP2[ind] <- "00066"

#clean master (if there were duplicate WP measurements)
foo <- master |> 
  group_by(DATE, TIME, HOUSE, PLOT) |> 
  summarize(n = n()) |> 
  filter(n == 2)

to_remove <- master |> 
  inner_join(foo) |> 
  filter(DUP == 1)

master <- master |> 
  anti_join(to_remove) |> 
  mutate()

# Fix treatments associated with 
goo <- master |> 
   mutate(ID = paste0("H", HOUSE, "P", PLOT)) |> 
  group_by(ID, TREAT) |> 
  summarize(n = n()) |> 
   filter(n != 1)

master <- master |> 
  mutate(ID = paste0("H", HOUSE, "P", PLOT)) |> 
  select(-TREAT) |> 
  left_join(goo) |> 
  select(-n)

table(master$ID, master$TREAT)

# loop to read, calculate, and combine spectra by date
for(i in 1:length(dates)){
  print(i)
  daily_master <- master[which(master$DATE == dates[i]), ]
  file_list_R1 <- c()
  # file_list_R2 <- c()
  time_id <- c()
  house_id <- c()
  plot_id <- c()
  treat_id <- c()
  winter_id <- c()
  summer_id <- c()
  rep_id <- c()
  full_id <- c()
  scan_id_R1 <- c()
  # scan_id_R2 <- c()
  date_id <- c()
  wp_id <- c()
  rwc_id <- c()
  
  # Loop to determine REP1 files for a particular date
  for(j in 1:length(daily_master$REP1)){
    file_list_R1[j] <- list.files(paste0("data/hyperspec_0/", folders[i], '/'), 
                               full.names = T, 
                               pattern = paste0(daily_master$REP1[j], ".asd"))
    scan_id_R1[j] <- sub(".*/", "", file_list_R1[j])
    date_id[j] <- daily_master$DATE[j]
    time_id[j] <- daily_master$TIME[j]
    house_id[j] <- paste0("H", daily_master$HOUSE[j])
    plot_id[j] <- paste0("P", daily_master$PLOT[j])
    treat_id[j] <- daily_master$TREAT[j]
    winter_id[j] <- substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[1]
    summer_id[j] <- substring(daily_master$TREAT[j], seq(1, 3, 2), seq(2, 4, 2))[2]
    # rep_id[j] <- daily_master$DUP[j]
    full_id[j] <- paste0(house_id[j], plot_id[j], treat_id[j], time_id[j], rep_id[j])
    wp_id[j] <- daily_master$WP[j]
    rwc_id[j] <- daily_master$RWC[j]
  }
  
  # For loop to read and append all REP1 scans to single df
  dataset_rep1 <- data.frame()
  for (j in 1:length(file_list_R1)){
    md <- get_metadata(file_list_R1[j])
    #read in hyperspectral data using 'asdreader' package
    temp_data <- asdreader::get_spectra(file_list_R1[j]) 
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
    rep <- rep("REP1", length(wvl))
    full <- rep(full_id[j], length(wvl))
    scan <- rep(scan_id_R1[j], length(wvl))
    
    ds <- cbind(date, wvl, data_s, time, 
                house, plot, treat, 
                winter, summer, rep, full, scan)
    dataset_rep1 <- rbind(dataset_rep1, ds)
  }
  print("rep1 complete")
  
  # file_list_R1 <- c()
  file_list_R2 <- c()
  time_id <- c()
  house_id <- c()
  plot_id <- c()
  treat_id <- c()
  winter_id <- c()
  summer_id <- c()
  rep_id <- c()
  full_id <- c()
  # scan_id_R1 <- c()
  scan_id_R2 <- c()
  date_id <- c()
  wp_id <- c()
  rwc_id <- c()
  
  
    # Loop to determine REP2 for a particular date
  for(j in 1:length(daily_master$REP2)){
    file_list_R2[j] <- list.files(paste0("data/hyperspec_0/", folders[i], '/'), 
                                  full.names = T, 
                                  pattern = paste0(daily_master$REP2[j], ".asd"))
    scan_id_R2[j] <- sub(".*/", "", file_list_R2[j])
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
  
  # For loop to read and append all REP2 scans to single df
  dataset_rep2 <- data.frame()
  for (j in 1:length(file_list_R2)){
    md <- get_metadata(file_list_R2[j])
    #read in hyperspectral data using 'asdreader' package
    temp_data <- asdreader::get_spectra(file_list_R2[j]) 
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
    rep <- rep("REP2", length(wvl))
    full <- rep(full_id[j], length(wvl))
    scan <- rep(scan_id_R2[j], length(wvl))
    
    ds <- cbind(date, wvl, data_s, time, 
                house, plot, treat, 
                winter, summer, rep, full, scan)
    dataset_rep2 <- rbind(dataset_rep2, ds)
  }
  print("rep2 complete")
  
  
  # Stack Rep1 and Rep2
  dataset <- rbind.data.frame(dataset_rep1, dataset_rep2)
  
  #Name the columns
  names(dataset) <- c("date", "wavelength", "reflectance", "time", 
                      "house", "plot", "treat", "winter", "summer", "rep",
                      "full", "scan")
  dataset <- transform(dataset, wavelength = as.numeric(wavelength))
  dataset <- transform(dataset, reflectance = as.numeric(reflectance))
  
  # Write out
 temp <- date[i] |> as.POSIXct(format = "%m/%d/%Y") |> as.character()
 col_date <- gsub("-", "", temp)
  write_csv(dataset, file = paste0("data/hyperspec_0/spectra_", col_date, ".csv"))
  
}


# Group together
fls <- list.files("data/hyperspec_0/",
                  pattern = ".csv")
spec_list <- list()
for(i in 1:length(fls)) {
  spec_list[[i]] <- read_csv(paste0("data/hyperspec_0/", fls[i]))
}
spec_df <- do.call(rbind, spec_list) |> 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         ID = paste0(house, plot),
         house = str_extract_all(house, "\\d+") |> 
           as.numeric(),
         plot = str_extract_all(house, "\\d+") |> 
           as.numeric()) |> 
  rename(trt_s = summer,
         trt_w = winter,
         trt = treat,
         period = time,
         date_col = date) 

# plot spectra for replicates before averages
to_loop <- spec_df |> 
  group_by(date_col, trt_s) |> 
  summarize(n = n())

for(i in 1:nrow(to_loop)){
  fig <- spec_df |> 
    filter(date_col == to_loop$date_col[i],
           trt_s == to_loop$trt_s[i]) |> 
    ggplot() +
    geom_line(aes(x = wavelength, y = reflectance, 
                  color = as.factor(rep))) + 
    facet_grid(rows = vars(period),
               cols = vars(ID))
  
  ggsave(paste0("figures/spectral_replicates/", to_loop$date_col[i],"-", to_loop$trt_s[i], ".png"),
         width = 8,
         height = 5,
         units = "in")
}

# Largely seems okay to average across
spec_avg <- spec_df |> 
  group_by(date_col, ID, period, wavelength) |> 
  summarize(refl_mean = mean(reflectance),
            n = n())

# Join with wp and rwc
wp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")

spec_avg_wp <-spec_avg |> 
  left_join(wp)

write_csv(spec_avg_wp, file = "data_clean/spectra_wp.csv")


# Check reflectance max 40% in NIR, red and blue min should be almost 0
# Vector normalized reflectance (relative differences in shape)
# Keep 400 to 2500


# Plot spectra

ggplot(spec_avg_wp) +
  geom_line(aes(x = wavelength, y = refl_mean,
                color = date_col)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period))
  
ggplot(dataset, 
       aes(x = wavelength, y = reflectance,
           group = full, color = time)) +
  geom_line(show.legend = T, linewidth =.8, linetype = "solid") +
  facet_wrap(~date) +
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)", 
                     limits = c(400, 2400), 
                     breaks = seq(400, 2400, 400)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  facet_wrap(~summer, ncol = 1) +
  theme(text = element_text(size = 40)) +
  theme_bw()
