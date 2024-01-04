library(googlesheets4)
library(tidyverse)


# Read in water potential
WP <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                 sheet = "Water potential",
                 col_types = "Dcnncnctnc") %>% # time type not implemented yet
      filter(date_col >= as.Date("2023-08-14")) |> 
  mutate(ID = paste0("H", house, "P", plot))

rwc <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                     sheet = "RWC") %>%
  mutate(mass_fresh = vial_cap_fresh_g - vial_cap_g,
         mass_dry = vial_dry_g - vial_g,
         RWC = (mass_fresh - mass_dry) / (saturated_g - mass_dry),
         date_col = as.Date(date_col)) %>%
  filter(date_col >= as.Date("2023-08-14"))

### Test which duplicate to remove
dups <- WP |> 
  select(-time_wp, -notes, -cID) |> 
  group_by(date_col, period, house, plot) |> 
  count() |> 
  filter(n > 1) # 2 sets of midday duplicates

# Combine RWC with WP set
both_t <- cbind.data.frame(rwc, wp_mpa = WP$wp_mpa)

# plot duplicate middays against  predawns for all - which is a better fit?
set1 <- both_t |> 
  filter(date_col == dups$date_col[1],
         house == dups$house[1],
         plot == dups$plot[1])


set2 <- both_t |> 
  filter(date_col == dups$date_col[2],
         house == dups$house[2],
         plot == dups$plot[2])

# duplicated WPs are excluded with RWC filters between 0.25 and 1, no need for additional cleaning

both_t %>%
  filter(RWC > 0.25 & RWC < 1) %>%
  ggplot(aes(x = RWC, y = wp_mpa,
             color = period)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1)

both_t %>%
  mutate(trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d")) %>%
  filter(RWC > 0.25 & RWC < 1) %>%
  ggplot(aes(x = RWC, y = wp_mpa,
             color = period)) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_vline(xintercept = 1) +
  facet_grid(rows = vars(trt_s),
             cols = vars(trt_w))

both_t %>%
  mutate(trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d")) %>%
  filter(RWC > 0.25 & RWC < 1) %>%
  ggplot(aes(x = 1-RWC, y = -1/wp_mpa,
             shape = period,
             color = trt_s)) +
  geom_point() +
  facet_wrap(~date_col) +
  scale_y_continuous(expression(paste("1/", Psi, " (-MPa)"))) +
  theme_bw(base_size = 12)

# Quick plot to check WP-RWC relationship
both_t |>
  filter(date_col >= as.Date("2023-08-14"),
         RWC < 1, 
         RWC > 0) |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  ggplot(aes(x = RWC, y = wp_mpa)) +
  geom_point(aes(col = summer)) + 
  scale_x_continuous(("RWC (g/g)")) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  facet_wrap(~date_col) +
  # facet_wrap(~period) + 
  theme_bw()


# Check by treatment over time

WP_sum <- both_t |> 
  mutate(summer = str_extract(trt, "S[0-9]"),
         winter = str_extract(trt, "W\\d")) |> 
  group_by(date_col, summer, winter, period) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))))


WP_sum_plot <- WP_sum |> 
  filter(date_col >= as.Date("2023-08-14"))

WP_plot <- WP |> 
  mutate(summer = str_extract(trt, "S[0-9]"),
         winter = str_extract(trt, "W\\d")) |> 
  filter(date_col >= as.Date("2023-08-14"))

ggplot() +
  geom_point(data = WP_plot,
             aes(x = date_col,
                 y = wp_mpa,
                 color = period),
             alpha = 0.15,
             position = "jitter") +
  geom_pointrange(data = WP_sum_plot,
                  aes(x = date_col,
                      ymin = WP_m - WP_sd,
                      y = WP_m,
                      ymax = WP_m + WP_sd,
                      color = period),
                  size = 0.3) +
  geom_line(data = WP_sum_plot,
            aes(x = date_col,
                y = WP_m,
                color = period)) +
  facet_grid(cols = vars(summer),
             rows = vars(winter)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  theme_bw() +
  theme(axis.title.x = element_blank())
 
RWC_sum <- rwc |> 
  filter(RWC < 1,
         RWC > 0) |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  group_by(date_col, summer, period) |> 
  summarize(RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC))))

RWC_sum_plot <- RWC_sum |> 
  filter(date_col >= as.Date("2023-08-14")) 

RWC_plot <- rwc |> 
  filter(RWC < 1,
         RWC > 0) |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  filter(RWC < 5) |> 
  filter(date_col >= as.Date("2023-08-14")) 

ggplot() +
  geom_point(data = RWC_plot,
             aes(x = date_col,
                 y = RWC,
                 color = period),
             alpha = 0.15,
             position = "jitter") +
  geom_pointrange(data = RWC_sum_plot,
                  aes(x = date_col,
                      ymin = RWC_m - RWC_sd,
                      y = RWC_m,
                      ymax = RWC_m + RWC_sd,
                      color = period),
                  size = 0.25) +
  geom_line(data = RWC_sum_plot,
            aes(x = date_col,
                y = RWC_m,
                color = period)) +
  facet_wrap(~summer) +
  scale_x_date(date_labels = "%m/%d") +
  scale_y_continuous("RWC (g/g)") +
  theme_bw() +
  theme(axis.title.x = element_blank())

# Make final combined dataset to write out
wp_rwc <- cbind.data.frame(rwc, wp_mpa = WP$wp_mpa) |> 
  mutate(trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d")) |> 
  filter(RWC > 0.25 & RWC < 1) |> 
  select(-7:-18)

wp_rwc |> 
  ggplot(aes(x = RWC, y = wp_mpa)) +
  geom_point(aes(color = period)) +
  facet_wrap(~trt_s)

# Make wide with predawns and middays
wp_wide <- wp_rwc |> 
  select(-cID, -RWC) |> 
  pivot_wider(names_from = period,
              values_from = wp_mpa) # has missing pairings due to RWC filtering

# Make wide only eliminating second duplicates
dups <- WP |> 
  select(-time_wp, -notes, -cID) |> 
  group_by(date_col, period, house, plot) |> 
  count() |> 
  filter(n > 1)

# rows to remove
ind1 <- which(WP$date_col == dups$date_col[1] &
      WP$period == dups$period[1] &
      WP$house == dups$house[1] &
      WP$plot == dups$plot[1])[1]

ind2 <- which(WP$date_col == dups$date_col[2] &
                WP$period == dups$period[2] &
                WP$house == dups$house[2] &
                WP$plot == dups$plot[2])[1]

wp_wide2 <- WP[-1*c(ind1, ind2),] |> 
  select(-plant, -cID, -time_wp, -notes) |> 
  pivot_wider(names_from = period,
              values_from = wp_mpa) |> 
  mutate(trt_s = str_extract(trt, "S\\d"),
         trt_w = str_extract(trt, "W\\d"))

wp_wide2 |> 
  ggplot(aes(x = predawn,
             y = midday,
             color = trt_s)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2)

#### Add days since pulse by treatment

irig <- read_csv("data/irrig.csv",
                 locale = locale(tz = "America/Phoenix"))

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

pulse_days <- irig_long %>%
  group_by(trt_s) %>%
  mutate(last_event = as.Date(ifelse(irig > 0, date, NA_real_), origin = "1970-01-01")) %>%
  fill(last_event) %>%
  mutate(days_since_pulse = as.numeric(date - last_event))

# match days since pulse to  wp_rwc and wp_wide2

wp_rwc_out <- wp_rwc |> 
  left_join(pulse_days, by = join_by("date_col" == "date", "trt_s")) |> 
  left_join(pulse_num, by = join_by("date_col" == "date", "trt_s"))

wp_rwc_out |> 
  ggplot(aes(x = days_since_pulse)) +
  geom_point(aes(y = wp_mpa,
                 color = as.factor(pulse_num))) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period))
   