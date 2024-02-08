# Timeseries plots to prep for model
library(tidyverse)

# read in  cleaned datasets
wp_rwc <- read_csv("data_clean/wp_rwc.csv")|> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(13, 7, 3) ~ 1,
                                pulse_num %in% c(15, 8) ~ 2))

wp_wide <- read_csv("data_clean/wp_wide.csv") |> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(13, 7, 3) ~ 1,
                                pulse_num %in% c(15, 8) ~ 2))


# plot WP only over days
wp_wide |> 
  ggplot(aes(x = days_since_pulse, y = predawn)) +
  geom_point() +
  geom_line(aes(color = ID)) +
  facet_grid(rows = vars(pulse_num2),
             cols = vars(trt_s),
             scales = "free_x",
             space = "free")

wp_wide |> 
  ggplot(aes(x = days_since_pulse, y = midday)) +
  geom_point() +
  geom_line(aes(color = ID)) +
  facet_grid(rows = vars(pulse_num2),
             cols = vars(trt_s),
             scales = "free_x",
             space = "free")

# plot WP and RWC over days
wp_rwc |> 
  ggplot(aes(x = days_since_pulse, y = wp_mpa)) +
  geom_point(aes(color = period)) +
  geom_line(aes(group = ID)) +
  facet_grid(cols = vars(pulse_num2, trt_s),
             rows = vars(period),
             scales = "free_x",
             space = "free")

wp_rwc |> 
  ggplot(aes(x = days_since_pulse, y = RWC)) +
  geom_point(aes(color = period)) +
  geom_line(aes(group = ID)) +
  facet_grid(cols = vars(pulse_num2, trt_s),
             rows = vars(period),
             scales = "free_x",
             space = "free")

wp_rwc |> 
  ggplot(aes(x = days_since_pulse, y = wp_mpa)) +
  geom_point(aes(color = as.factor(pulse_num2))) +
  # geom_line(aes(group = ID)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period),
             scales = "free_x",
             space = "free")

wp_rwc |> 
  ggplot(aes(x = days_since_pulse, y = RWC)) +
  geom_point(aes(color = as.factor(pulse_num2))) +
  # geom_line(aes(group = ID)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period),
             scales = "free_x",
             space = "free")

# explore distributions of S4
wp_rwc |> 
  filter(trt_s == "S4") |> 
  ggplot() +
  geom_histogram(aes(x = wp_mpa))

wp_rwc |> 
  filter(trt_s == "S4") |> 
  ggplot() +
  geom_histogram(aes(x = RWC))

# daytime vpd
vpd_day <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  rename("interval" = "period") |> 
  filter(interval == "day", 
         location == "inside") |> 
  select(-location, -interval, -sd) |> 
  rename("Dmean_day" = mean)

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
# use morning VWC, between 4:30 am and 12 pm
vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  rename("interval" = "period") |> 
  filter(interval == "morn") |> 
  mutate(depth2 = case_when(depth == "0-12 cm" ~ "SWC_1",
                            depth == "25 cm" ~ "SWC_2",
                            depth == "75 cm" ~ "SWC_3")) |> 
  select(-sd, -depth, -interval) |> 
  pivot_wider(names_from = depth2,
              values_from = mean)
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(date >= min(wp_wide$date_col),
         date <= max(wp_wide$date_col))
cde <- read_csv("data_clean/cde_daytime_pulse.csv") |> 
  select(date, trt_s, cde)

# join together for plotting
 wp_all <- wp_wide |> 
   left_join(vpd_day, by = join_by("date_col" == "date")) |> 
   left_join(vpd_int, by = join_by("date_col" == "date")) |> 
   left_join(vwc, by = join_by("date_col" == "date", "trt_s" == "summer")) |> 
   # left_join(irig, by = join_by("date_col" == "date", "trt_s")) |> 
   left_join(cde, by = join_by("date_col" == "date", "trt_s"))  |> 
   mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                 pulse_num %in% c(8, 15) ~ "Aug 21") |> 
            as.factor())

 
# Quick scatterplots
# can't really use CDE, as it indexes time calculated by pulse and is not generally available
wp_all |> 
  ggplot() +
  geom_point(aes(x = date_col, y = predawn)) +
  geom_line(aes(x = date_col, y = SWC_2*10, col = "VWC 25cm")) +
  facet_wrap(~trt_s)
 
 wp_all |> 
   ggplot() +
   geom_point(aes(x = Dmean_PD, y = predawn, col = factor(house))) +
   facet_grid(cols = vars(trt_s))

wp_all |> 
  ggplot() +
  geom_point(aes(x = Dmean_MD, y = midday, col = factor(days_since_pulse))) +
  facet_grid(rows = vars(pulse_num2),
             cols = vars(trt_s))
 
 
 wp_all |> 
   ggplot() +
   geom_point(aes(x = SWC_2, y = midday, col = factor(days_since_pulse))) +
   facet_wrap(~trt_s)
 
 wp_all |> 
   ggplot() +
   geom_point(aes(x = SWC_3, y = midday, col = factor(days_since_pulse))) +
   facet_grid(rows = vars(pulse_num2),
              cols = vars(trt_s))
 
 ### Check VPD options
 # daytime for now, consider using PD and MD
 vpd_all <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
   rename("interval" = "period") |> 
   filter(location == "inside") |> 
   select(-location, -sd) |> 
   rename("Dmean" = mean)
 
 vpd_all |> 
   ggplot(aes(x = date, y = Dmean,
              color = interval)) +
   geom_point() +
   geom_line() +
   theme_bw()

 vpd_wide <- vpd_all |> 
   filter(interval %in% c("PD", "MD"),
          !is.na(Dmean)) |> 
   pivot_wider(names_from = interval,
               values_from = Dmean)
cor(vpd_wide$PD, vpd_wide$MD) 
cor(wp_wide$predawn, wp_wide$midday)
cor(wp_all$SWC_1, wp_all$SWC_2)
cor(wp_all$SWC_2, wp_all$SWC_3)
cor(wp_all$SWC_1, wp_all$SWC_3)

wp_long <- wp_wide |> 
  rename("PD" = "predawn", "MD" = "midday") |> 
  pivot_longer(cols = c("PD", "MD"),
               names_to = "period",
               values_to = "WP") |> 
  left_join(vpd_all |> filter(interval %in% c("PD", "MD")),
            by = join_by("date_col" == "date",
                         "period" == "interval")) |> 
  rename("Dmean_period" = "Dmean") |> 
  left_join(vpd_day, by = join_by("date_col" == "date")) |> 
  left_join(vwc, by = join_by("date_col" == "date", "trt_s" == "summer"))

wp_long |> 
  ggplot(aes(x = Dmean_day, y = WP, col = period)) +
  geom_point() +
  facet_wrap(~trt_s)

wp_long |> 
  ggplot(aes(x = Dmean_period, y = WP, col = period)) +
  geom_point(aes(shape = factor(house))) +
  facet_grid(cols = vars(trt_s))

wp_long |> 
  ggplot(aes(x = SWC_1, y = WP, col = period)) +
  geom_point(aes(shape = factor(house))) +
  facet_grid(cols = vars(trt_s))

wp_long |> 
  ggplot(aes(x = SWC_1, y = WP, col = trt_s)) +
  geom_point(aes(shape = factor(house))) +
  facet_wrap(~period)


wp_long |> 
  ggplot(aes(x = Dmean_day, y = WP, col = trt_s)) +
  geom_point(aes(shape = factor(house))) +
  facet_wrap(~period, scales = "free_x")

# check for house REs
wp_long |> 
  group_by(date_col, house, period) |> 
  summarize(WP_m = mean(WP),
            WP_sd = sd(WP)) |> 
  ggplot(aes(x = date_col, col = factor(house), shape = period)) +
  geom_point(aes(y = WP_m)) +
  geom_line(aes(y = WP_m)) +
  geom_errorbar(aes(ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd),
                width = 0)


### Save out
write_csv(wp_wide, "scripts/mod1 - pd/data_wide.csv")
write_csv(wp_long, "scripts/mod1 - pd/data_long.csv")
