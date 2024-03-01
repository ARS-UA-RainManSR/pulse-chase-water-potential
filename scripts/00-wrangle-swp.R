# Compile/summarize SWP data (most sensors didn't start working until after the pulse chase)

# Only H3 was available during 2023 pulse chase

library(tidyverse)
library(readxl)
library(udunits2)

##### Processing H3 only for pulse chase #####
# Read in H3
temp <- read_csv("data/swp/CR1000_SWP_H3_SWP_PulseChase.dat",
                 skip = 1,
                 n_max = 1)

h3 <- read_csv("data/swp/CR1000_SWP_H3_SWP_PulseChase.dat",
               skip = 3,
               locale = locale(tz = "America/Phoenix"))
colnames(h3) <- colnames(temp)

h3 <-  h3 |> 
  rename(H3P6_25cm_SWP = H3P6_25cm_SWC) # typo

# Read in treatments
trt <- read_xlsx("data/treatments.xlsx") |> 
  mutate(ID = paste0("H", House, "P", Plot)) |> 
  relocate(ID) |> 
  select(-House, -Plot) |> 
  rename(trt_s = Summer, trt_w = Winter) |> 
  mutate(trt_s = paste0("S", trt_s),
         trt_w = paste0("W", trt_w))


swp_long <- h3 |> 
  select(-RECORD) |> 
  pivot_longer(-TIMESTAMP, 
               names_to = c("ID", "depth", "variable"),
               names_pattern = "(.*)_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = variable,
              values_from = value) |> 
  left_join(trt, by = join_by(ID)) |> 
  mutate(depth = case_when(depth == "10cm" ~ "10 cm",
                           depth == "25cm" ~ "25 cm"),
         SWP = ud.convert(SWP, "kPa", "MPa")) # Convert to megapascals

attr(swp_long$TIMESTAMP[1], "tzone")

# Check how many of each summer treatment (1 each in H3)
table(swp_long$ID, swp_long$trt_s)

# Summarize to daily,  
swp_trt_daily <- swp_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix")) |> 
  group_by(date, trt_s, depth) |> 
  summarize(swp_daily_mean = mean(SWP, na.rm = TRUE),
            swp_daily_sd = sd(SWP, na.rm = TRUE))

swp_trt_daytime <- swp_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 7, hr <= 19) |>  # set daytime as between 7 am and 7 pm
  group_by(date, trt_s, depth) |> 
  summarize(swp_day_mean = mean(SWP, na.rm = TRUE),
            swp_day_sd = sd(SWP, na.rm = TRUE))

swp_trt_morning <- swp_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 4.5, hr <= 12) |>  # set morning as between 4:30 am and 12 pm
  group_by(date, trt_s, depth) |> 
  summarize(swp_morn_mean = mean(SWP, na.rm = TRUE),
            swp_morn_sd = sd(SWP, na.rm = TRUE))

swp_trt_comb <- swp_trt_daily |> 
  left_join(swp_trt_daytime, by = join_by("date", "trt_s", "depth")) |> 
  left_join(swp_trt_morning, by = join_by("date", "trt_s", "depth")) |> 
  pivot_longer(starts_with("swp"),
               names_to = c("period", "type"),
               names_pattern = "swp_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = "type",
              values_from = "value")

swp_trt_comb |> 
  ggplot(aes(x = date, y = mean,
             color = period)) +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd,),
                alpha = 0.25,
                width = 0) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(depth),
             cols = vars(trt_s)) +
  theme_bw()

# Write out teros 21 data 
write_csv(swp_trt_comb, "data_clean/swp_teros_daily_daytime.csv")


##### Reading all available sensors #####

# Read in each dat file, by house
# H3
temp <- read_csv("data/swp/CR1000_SWP_H3_20231013.dat",
                 skip = 1,
                 n_max = 1)

h3 <- read_csv("data/swp/CR1000_SWP_H3_20231013.dat",
               skip = 3)
colnames(h3) <- colnames(temp)

h3 <-  h3 |> 
  rename(H3P6_25cm_SWP = H3P6_25cm_SWC) # typo

# H1
# contains test plots outside of house, treat separately
temp <- read_csv("data/swp/CR1000_SWP_H1_20231013.dat",
                 skip = 1,
                 n_max = 1)

h1 <- read_csv("data/swp/CR1000_SWP_H1_20231013.dat",
               skip = 3)
colnames(h1) <- colnames(temp)

# H5
temp <- read_csv("data/swp/CR1000_H5_SWP_20231013.dat",
                 skip = 1,
                 n_max = 1)

h5 <- read_csv("data/swp/CR1000_H5_SWP_20231013.dat",
               skip = 3)
colnames(h5) <- colnames(temp)

#### Extract test sensors
test_25 <- h1 |> 
  select(TIMESTAMP, starts_with("test")) |> 
  rename(Test_25cm_SWP_teros = Test_25cm_SWP,
         Test_25cm_VWC_655 = Test_25cm_T) |> 
  pivot_longer(-TIMESTAMP,
               names_to = c("depth", "variable", "sensor"),
               names_pattern = "Test_(.*)_(.*)_(.*)",
               values_to = "value") 

#### Pivot longer and merge

swp_wide <- h3 |> 
  left_join(select(h1, !starts_with("Test")), by = join_by("TIMESTAMP")) |> 
  left_join(h5, by = join_by("TIMESTAMP")) |> 
  select(-starts_with("RECORD"))

swp_long <- swp_wide |> 
  pivot_longer(-TIMESTAMP,
               names_to = c("ID", "depth", "variable"),
               names_pattern = "(.*)_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = variable,
              values_from = value)
         