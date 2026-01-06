library(tidyverse)
library(plantecophys)
env <- read_csv("data/env/Halfhourly_VWC_SWP_Weather_092323.csv") |> 
  select(TIMESTAMP, T_inside, RH_inside, T_outside, RH_outside) |> 
  mutate(D_inside = RHtoVPD(RH_inside, T_inside),
         D_outside = RHtoVPD(RH_outside, T_outside),
         date = as.Date(TIMESTAMP)) |> 
  filter(date == as.Date("2023-08-21"))

which.min(env$D_inside)
which.min(env$D_outside)

vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(period == "morn",
         location == "inside",
         date >= as.Date("2023-08-14"),
         date <= as.Date("2023-09-04"))
which.min(vpd$mean)


