# Bivariate plots to prep for model
# Revamped for WP only, newly cleaned data

library(tidyverse)

# read in  cleaned datasets
wp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")

# PD and MD VPD
vpd_int <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  rename("interval" = "period") |> 
  filter(location == "inside",
         interval %in% c("PD", "MD"),
         !is.na(mean)) |>  
  select(-location, -sd) |> 
   pivot_wider(names_from = interval,
               values_from = mean) |> 
  rename("Dmean_PD" = "PD", "Dmean_MD" = "MD")

# use morning VWCP, between 4:30 am and 12 pm
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  rename("interval" = "period") |> 
  filter(interval == "morn") |> 
  mutate(depth2 = case_when(depth == "0-12 cm" ~ "SWP_1",
                            depth == "25 cm" ~ "SWP_2",
                            depth == "75 cm" ~ "SWP_3")) |> 
  select(-sd, -depth, -interval) |> 
  pivot_wider(names_from = depth2,
              values_from = mean)

irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(date >= min(wp$date_col),
         date <= max(wp$date_col))

cde <- read_csv("data_clean/cde_daytime_pulse.csv") |> 
  select(date, trt_s, cde)

# join together for plotting
 wp_all <- wp |> 
   left_join(vpd_int, by = join_by("date_col" == "date")) |> 
   left_join(swp, by = join_by("date_col" == "date", "trt_s" == "summer")) |> 
   # left_join(irig, by = join_by("date_col" == "date", "trt_s")) |> 
   # left_join(cde, by = join_by("date_col" == "date", "trt_s"))  |> 
   mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                 pulse_num %in% c(8, 15) ~ "Aug 21") |> 
            as.factor(),
          period = factor(period, levels = c("predawn", "midday")),
          Dmean = case_when(period == "predawn" ~ Dmean_PD,
                            period == "midday" ~ Dmean_MD))

 
# Quick scatterplots

# Check timeseries
wp_all |> 
  ggplot() +
  geom_point(aes(x = date_col, y = value)) +
  geom_line(aes(x = date_col, y = SWP_1, col = "SWP 0-10 cm")) +
  facet_wrap(~trt_s)
 
 
# WP vs. Dmean by period, day zero annotated
wp_all |> 
   ggplot() +
   geom_point(aes(x = Dmean, y = value, col = period)) +
   geom_point(data = wp_all |> filter(days_since_pulse == 0),
              aes(x = Dmean, y = value, col = period),
              shape = 8) +
   facet_grid(cols = vars(trt_s))

# WP vs. Dmean by period, colored by SWP_1
wp_all |> 
  ggplot() +
  geom_point(aes(x = Dmean, y = value, col = SWP_1, shape = period)) +
  geom_point(data = wp_all |> filter(days_since_pulse == 0),
             aes(x = Dmean, y = value, col = SWP_1),
             shape = 8) 
  # facet_grid(# cols = vars(trt_s),
             # rows = vars(period))

# Regardless of treatment, WP vs. SWP shallow by period
 wp_all |> 
   ggplot() +
   geom_point(aes(x = SWP_1, y = value, col = trt_s)) +
   geom_point(data = wp_all |> filter(days_since_pulse == 0),
              aes(x = SWP_1, y = value, col = trt_s),
              shape = 8) +
   facet_wrap(~period, ncol = 1)
 
 # Test interaction of SWP and VPD
 # WP vs. Dmean by period, color is VPD
 wp_all |> 
   filter(period == "predawn") |> 
   ggplot() +
   geom_point(aes(x = SWP_1, y = value, col = Dmean)) +
   geom_point(data = wp_all |> filter(days_since_pulse == 0,
                                      period == "predawn"),
              aes(x = SWP_1, y = value, col = Dmean),
              shape = 8)
 
 # Regardless of treatment, WP vs. SWP mid by period
 wp_all |> 
   ggplot() +
   geom_point(aes(x = SWP_2, y = value, col = trt_s)) +
   geom_point(data = wp_all |> filter(days_since_pulse == 0),
              aes(x = SWP_2, y = value, col = trt_s),
              shape = 8) +
   facet_wrap(~period, ncol = 1)
# No standard relationship, because highly variable SWP_2 by treatment
# Only S4 seems to have a standard relationship
 
 
# Write out joined data
 save(wp_all, file = "scripts/mod1 - wp/wp_all.Rdata")
 