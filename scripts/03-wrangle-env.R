# Read in env data
library(readxl)
library(tidyverse)
library(plantecophys)
source("source/vwc2swp.R")

trt <- read_excel("data/treatments.xlsx")

env <- read_csv("data/env/Halfhourly_VWC_SWP_Weather_092323.csv",
                locale = locale(tz = "America/Phoenix")) |> 
  filter(TIMESTAMP >= as.POSIXct("2023-08-14 00:00:00", 
                                 tz = "America/Phoenix"))

pulse_num <- read_csv("data_clean/pulse_num_days.csv")
#### Create VPD data half-hourly

vpd <- env |> 
  select(TIMESTAMP, ends_with("inside"), ends_with("outside")) |> 
  mutate(D_inside = RHtoVPD(RH_inside, T_inside),
         D_outside = RHtoVPD(RH_outside, T_outside))

vpd_daily <- vpd |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix")) |> 
  group_by(date) |> 
  summarize(D_inside_daily_mean = mean(D_inside, na.rm = TRUE),
            D_inside_daily_sd = sd(D_inside, na.rm = TRUE),
            D_outside_daily_mean = mean(D_outside, na.rm = TRUE),
            D_outside_daily_sd = sd(D_outside, na.rm = TRUE))
  
vpd_daytime <- vpd |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 7, hr <= 19) |>  # set daytime as between 7 am and 7 pm
  group_by(date) |> 
  summarize(D_inside_day_mean = mean(D_inside, na.rm = TRUE),
            D_inside_day_sd = sd(D_inside, na.rm = TRUE),
            D_outside_day_mean = mean(D_outside, na.rm = TRUE),
            D_outside_day_sd = sd(D_outside, na.rm = TRUE))

vpd_PD <- vpd |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 4, hr <= 6) |>  # set PD as between 4 am and 6 am
  group_by(date) |> 
  summarize(D_inside_PD_mean = mean(D_inside, na.rm = TRUE),
            D_inside_PD_sd = sd(D_inside, na.rm = TRUE),
            D_outside_PD_mean = mean(D_outside, na.rm = TRUE),
            D_outside_PD_sd = sd(D_outside, na.rm = TRUE))

vpd_MD <- vpd |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 11, hr <= 13) |>  # set MD as between 11 am and 1 pm
  group_by(date) |> 
  summarize(D_inside_MD_mean = mean(D_inside, na.rm = TRUE),
            D_inside_MD_sd = sd(D_inside, na.rm = TRUE),
            D_outside_MD_mean = mean(D_outside, na.rm = TRUE),
            D_outside_MD_sd = sd(D_outside, na.rm = TRUE))

vpd_comb <- vpd_daily |> 
  left_join(vpd_daytime, by = join_by("date")) |> 
  left_join(vpd_PD, by = join_by("date")) |> 
  left_join(vpd_MD, by = join_by("date")) |> 
  pivot_longer(starts_with("D_"),
               names_to = c("location", "period", "type"),
               names_pattern = "D_(.*)_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = "type",
              values_from = "value")

vpd_comb |> 
  ggplot(aes(x = date, y = mean,
             color = period)) +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd,),
                alpha = 0.25,
                width = 0) +
  geom_point() +
  geom_line() +
  facet_wrap(~location) +
  theme_bw()

#### Create CDE cumulative D exposure for daytime/inside
cde <- pulse_num |> 
  left_join(vpd_daytime |> select(1:2),
            by = join_by("date")) |> 
  drop_na() |> 
  rename(vpd = D_inside_day_mean) |> 
  mutate(excess_D = if_else(vpd > 1, vpd - 1, 0)) |> 
  # filter(pulse_num == 3) |> 
  group_by(trt_s, pulse_num) |> 
  mutate(cde = cumsum(excess_D))

ggplot(cde) +
  geom_point(aes(x = date,
                 y = cde)) +
  facet_wrap(~pulse_num)

#### Create VWC data by treatment 
colnames(env)

vwc_long <- env |> 
  select(TIMESTAMP, starts_with("VWC")) |> 
  pivot_longer(-TIMESTAMP,
               names_to = c("plot1", "depth1", "house1"),
               names_pattern = "VWC_(.*)_(.*)_(.*)",
               values_to = "VWC") |> 
  mutate(ID = paste0(house1, plot1),
         plot = as.numeric(str_extract(plot1, "\\d")),
         house = as.numeric(str_extract(house1, "\\d")),
         depth = case_when(depth1 == 1 ~ "0-12 cm",
                           depth1 == 2 ~ "25 cm",
                           depth1 == 3 ~ "75 cm")) |> 
  select(-ends_with("1")) |> 
  left_join(trt, by = join_by(house == House, 
                              plot == Plot)) |> 
  mutate(trt = paste0("W", Winter, "S", Summer),
         summer = paste0("S", Summer)) |> 
  select(-Winter, -Summer) |> 
  mutate(SWP = case_when(depth == "0-12 cm" ~ vwc2swp(VWC, param = "depth10cm"),
                         depth == "25 cm" ~ vwc2swp(VWC, param = "depth25cm"),
                         depth == "75 cm" ~ vwc2swp(VWC, param = "depth25cm")))

vwc_trt_daily <- vwc_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix")) |> 
  group_by(date, summer, depth) |> 
  summarize(vwc_daily_mean = mean(VWC, na.rm = TRUE),
            vwc_daily_sd = sd(VWC, na.rm = TRUE),
            swp_daily_mean = mean(SWP, na.rm = TRUE),
            swp_daily_sd = sd(SWP, na.rm = TRUE))

vwc_trt_daytime <- vwc_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 7, hr <= 19) |>  # set daytime as between 7 am and 7 pm
  group_by(date, summer, depth) |> 
  summarize(vwc_day_mean = mean(VWC, na.rm = TRUE),
            vwc_day_sd = sd(VWC, na.rm = TRUE),
            swp_day_mean = mean(SWP, na.rm = TRUE),
            swp_day_sd = sd(SWP, na.rm = TRUE))

vwc_trt_morning <- vwc_long |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix"),
         hr = hour(TIMESTAMP) + minute(TIMESTAMP)/60) |> 
  filter(hr >= 4.5, hr <= 12) |>  # set morning as between 4:30 am and 12 pm
  group_by(date, summer, depth) |> 
  summarize(vwc_morn_mean = mean(VWC, na.rm = TRUE),
            vwc_morn_sd = sd(VWC, na.rm = TRUE),
            swp_morn_mean = mean(SWP, na.rm = TRUE),
            swp_morn_sd = sd(SWP, na.rm = TRUE))

vwc_trt_comb <- vwc_trt_daily |> 
  left_join(vwc_trt_daytime, by = join_by("date", "summer", "depth")) |> 
  left_join(vwc_trt_morning, by = join_by("date", "summer", "depth")) |> 
  select(-starts_with("swp")) |> 
  pivot_longer(starts_with("vwc"),
               names_to = c("period", "type"),
               names_pattern = "vwc_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = "type",
              values_from = "value")

vwc_trt_comb |> 
  ggplot(aes(x = date, y = mean,
             color = period)) +
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd,),
                alpha = 0.25,
                width = 0) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(depth),
             cols = vars(summer)) +
  theme_bw()

swp_trt_comb <- vwc_trt_daily |> 
  left_join(vwc_trt_daytime, by = join_by("date", "summer", "depth")) |> 
  left_join(vwc_trt_morning, by = join_by("date", "summer", "depth")) |> 
  select(-starts_with("vwc")) |> 
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
             cols = vars(summer)) +
  theme_bw()

# write out

write_csv(vpd_comb, "data_clean/vpd_daily_daytime.csv")
write_csv(vwc_trt_comb, "data_clean/vwc_daily_daytime.csv")
write_csv(swp_trt_comb, "data_clean/swp_daily_daytime.csv")
write_csv(cde, "data_clean/cde_daytime_pulse.csv")
