# Test correlation between SWP and VPD

library(tidyverse)

swp <- read_csv("data_clean/swp_daily_daytime.csv") |>
  filter(period == "morn",
         depth == "0-10 cm",
         !is.na(mean))
vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |>
  filter(location == "outside",
         period == "day",
         !is.na(mean))

swp <- swp |>
  left_join(vpd, by = join_by(date))


swp |>
  group_by(summer) |>
  summarize(cor = cor(mean.x, mean.y))
