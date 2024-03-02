# Clean wp and rwc
# Step 1: Read in data, filter for dups and outliers, join wide
# Step 2: Calculate pulse stats
# Step 3: Join together, pivot longer, output

library(googlesheets4)
library(tidyverse)

##### Read in and clean raw data, join as wide dataframe #####

# Read in water potential
wp <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                 sheet = "Water potential",
                 col_types = "Dcnncnctnc") |>  # time type not implemented yet
  filter(date_col >= as.Date("2023-08-14")) |> 
  mutate(ID = paste0("H", house, "P", plot),
         trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d")) |> 
  rename(time_col = time_wp, WP = wp_mpa)
           
  

rwc <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                  sheet = "RWC") |> 
  mutate(ID = paste0("H", house, "P", plot), 
         mass_fresh = vial_cap_fresh_g - vial_cap_g,
         mass_dry = vial_dry_g - vial_g,
         RWC = (mass_fresh - mass_dry) / (saturated_g - mass_dry),
         date_col = as.Date(date_col),
         trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d")) |> 
  filter(date_col >= as.Date("2023-08-14")) |> 
  rename(time_col = time_fresh)

# Obtain duplicates for anti-join
dups_wp <- wp |> filter(trt_s == "S4") |> 
  group_by(date_col, period, trt_s, ID) |> 
  count() |> 
  filter(n > 1) |> 
  select(-n) |> 
  inner_join(wp) |> 
  slice(1,3) # take the second of each measurement, first was more likely the error

dups_rwc <- rwc |> filter(trt_s == "S4") |> 
  group_by(date_col, period, trt_s, ID) |> 
  count() |> 
  filter(n > 1) |> 
  select(-n) |> 
  inner_join(rwc) |> 
  slice(1,3) # take the second of each measurement, first was more likely the error

# Anti_join to remove duplicates
wp2 <- wp |> 
  anti_join(dups_wp)

rwc2 <- rwc |> 
  anti_join(dups_rwc)

# Filter 1 for RWC: remove unbelievable values < 0.25 and > 1.0
rwc3 <- rwc2 |> 
  filter(RWC > 0.25 & RWC < 1)

# Filter 2 for RWC: remove MD values greater than min of PD values
# and vice versa: remove PD values less than max of MD values

# max/min for both periods
rwc_sum <- rwc3 |> 
  group_by(trt_s, period, date_col) |> 
  summarize(min_rwc = min(RWC),
            max_rwc = max(RWC)) |> 
  pivot_wider(names_from = period,
              values_from = c(max_rwc, min_rwc))

# Join and filter
rwc_pd <- rwc3 |> 
  filter(period == "predawn") |> 
  left_join(rwc_sum, by = join_by(trt_s, date_col)) |> 
  filter(RWC >= min_rwc_midday) |> # removed 9 predawns 
  select(-starts_with("max"), -starts_with("min"))

rwc_md <- rwc3 |> 
  filter(period == "midday") |> 
  left_join(rwc_sum, by = join_by(trt_s, date_col)) |> 
  filter(RWC <= max_rwc_predawn) |> # removed 6 middays 
  select(-starts_with("max"), -starts_with("min"))

# Rejoin filtered RWC predawn and middays, select matching column names
# 248 predawns versus 216 RWCs
rwc4 <- rwc_pd |> 
  bind_rows(rwc_md) |> 
  select(intersect(colnames(wp2), colnames(rwc3)), RWC)
  
# Combine as wide dataframe
both_wide <- wp2 |> 
  left_join(rwc4, by = join_by(date_col, period, house, plot, trt,
                               cID, plant, ID, trt_s, trt_w)) |> 
  rename(time_wp = time_col.x, time_rwc = time_col.y,
         notes_wp = notes.x, notes_rwc = notes.y)


##### Calculate pulse stats to join ##### 

#### Add days since pulse by treatment

irig <- read_csv("data/irrig.csv",
                 locale = locale(tz = "America/Phoenix")) |> 
  mutate(date = lubridate::mdy(date, tz = "America/Phoenix") |> 
           as.Date(tz = "America/Phoenix"))

# label pulse number
temp <- data.frame(S1 = numeric(nrow(irig)),
                   S2 = numeric(nrow(irig)),
                   S3 = numeric(nrow(irig)),
                   S4 = numeric(nrow(irig)))
for(i in 1:nrow(irig)) {
  if(irig$S1[i] > 0) {temp$S1[i] = 1} else {temp$S1[i] = 0}
  if(irig$S2[i] > 0) {temp$S2[i] = 1} else {temp$S2[i] = 0}
  if(irig$S3[i] > 0) {temp$S3[i] = 1} else {temp$S3[i] = 0}
  if(irig$S4[i] > 0) {temp$S4[i] = 1} else {temp$S4[i] = 0}
}

temp2 <- cumsum(temp)
temp2$date <- irig$date

pulse_num <- data.frame(date = seq(as.Date("2023-07-03", tz = "America/Phoenix"),
                                   as.Date("2023-09-04", tz = "America/Phoenix"),
                                   by = 1)) |> 
  left_join(temp2, by = join_by("date")) |> 
  fill(starts_with("S")) |> 
  pivot_longer(starts_with("S"),
               names_to = "trt_s",
               values_to = "pulse_num")

# create days since last pulse by treatment
irig_long <- data.frame(date = seq(as.Date("2023-07-03", tz = "America/Phoenix"),
                                   as.Date("2023-09-04", tz = "America/Phoenix"),
                                   by = 1)) |> 
  left_join(irig, by = join_by("date")) |> 
  replace_na(list(S1 = 0,
                  S2 = 0,
                  S3 = 0,
                  S4 = 0)) |> 
  pivot_longer(starts_with("S"),
               names_to = "trt_s",
               values_to = "irig")

pulse_days <- irig_long |> 
  group_by(trt_s) |> 
  mutate(last_event = as.Date(ifelse(irig > 0, date, NA_real_), origin = "1970-01-01")) |> 
  fill(last_event) |> 
  mutate(days_since_pulse = as.numeric(date - last_event)) |> 
  left_join(pulse_num, by = join_by("date", "trt_s"))

##### Join with pulse stats, pivot longer, and output #####
both_wide_pulse <- both_wide |> 
  left_join(pulse_days, by = join_by("date_col" == "date", "trt_s"))

both_long <- both_wide_pulse |> 
  pivot_longer(cols = c("WP", "RWC"),
               names_to = "variable",
               values_to = "value") |> 
  filter(!is.na(value))

write_csv(both_long, file = "data_clean/wp_rwc_long.csv")
