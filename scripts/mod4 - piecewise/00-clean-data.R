# Refine cleaning of long data by:
# removing the first WP of any duplicated measurements
# removing RWC if midday is higher an maximum predawn | predawn is lower than minimum miday

dat <- read_csv("data_clean/wp_rwc_long.csv")

dups <- dat |> filter(trt_s == "S4") |> 
  group_by(variable, period, trt_s, days_since_pulse, ID) |> 
  count() |> 
  filter(n > 1) |> 
  select(-n) |> 
  inner_join(dat) |> 
  slice(1,3) # take the second of each measurement, first was more likely the error


 dat |> 
  filter(trt_s == "S4",
         variable == "RWC") |> 
  ggplot(aes(x = days_since_pulse, y = value, color = period)) + 
  geom_point()

# max/min for both periods
rwc_sum <- dat |> 
  filter(trt_s == "S4",
         variable == "RWC") |> 
  group_by(period, days_since_pulse) |> 
  summarize(min_rwc = min(value),
            max_rwc = max(value)) |> 
  pivot_wider(names_from = period,
              values_from = c(max_rwc, min_rwc))

# Join and filter
rwc1 <- dat |> 
  filter(trt_s == "S4",
         variable == "RWC",
         period == "predawn") |> 
  left_join(rwc_sum, by = join_by(days_since_pulse)) |> 
  filter(value >= min_rwc_midday) |> # removed 1 predawn 
  select(-starts_with("max"), -starts_with("min"))
  
rwc2 <- dat |> 
  filter(trt_s == "S4",
         variable == "RWC",
         period == "midday") |> 
  left_join(rwc_sum, by = join_by(days_since_pulse)) |> 
  filter(value <= max_rwc_predawn) |> # removed 2 middays 
  select(-starts_with("max"), -starts_with("min"))

# Create new dataframe
dat2 <- dat |> 
  filter(trt_s == "S4",
         variable == "WP") |> 
  anti_join(dups) |> # remove second WP measurements
  bind_rows(rwc1) |> 
  bind_rows(rwc2) # 202 observations
dat |> filter(trt_s == "S4") |> nrow() # 207 observations

 
# Check plot
dat2 |>
  ggplot(aes(x = days_since_pulse, y = value)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  facet_grid(rows = vars(variable),
             cols = vars(period),
             scales = "free_y")

# save out
save(dat2, file = "scripts/mod4 - piecewise/s4_all.Rdata")
