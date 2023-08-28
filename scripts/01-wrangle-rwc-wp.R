library(googlesheets4)
library(tidyverse)


# Read in water potential
WP <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                 sheet = "Water potential")
# Read in RWC
rwc <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                     sheet = "RWC") %>%
  mutate(mass_fresh = vial_cap_fresh_g - vial_cap_g,
         mass_dry = vial_dry_g - vial_g,
         RWC = (mass_fresh - mass_dry) / (saturated_g - mass_dry)) %>%
  left_join(WP, 
            join_by(date_col, period, house, plot, trt, plant))

# Quick plot to check WP-RWC relationship
rwc |>
  filter(date_col >= as.Date("2023-08-14"),
         RWC < 5) |> 
  ggplot(aes(x = RWC, y = wp_mpa)) +
  geom_point(aes(col = period)) + 
  scale_x_continuous(("RWC (g/g)")) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  # facet_wrap(~date_col) +
  theme_bw()


# Check by treatment over time

WP_sum <- rwc |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  group_by(date_col, summer, period) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))))


WP_sum_plot <- WP_sum |> 
  filter(date_col >= as.Date("2023-08-14"),
         date_col <= as.Date("2023-08-26"))

WP_plot <- WP |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  filter(date_col >= as.Date("2023-08-14"),
         date_col <= as.Date("2023-08-26"))

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
                  size = 0.5) +
  geom_line(data = WP_sum_plot,
            aes(x = date_col,
                y = WP_m,
                color = period)) +
  facet_wrap(~summer) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  theme_bw() +
  theme(axis.title.x = element_blank())
 
RWC_sum <- rwc |> 
  filter(RWC < 5) |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  group_by(date_col, summer, period) |> 
  summarize(RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC))))

RWC_sum_plot <- RWC_sum |> 
  filter(date_col >= as.Date("2023-08-14"),
         date_col <= as.Date("2023-08-25")) 

RWC_plot <- rwc |> 
  mutate(summer = str_extract(trt, "S[0-9]")) |> 
  filter(RWC < 5) |> 
  filter(date_col >= as.Date("2023-08-14"),
         date_col <= as.Date("2023-08-25")) 

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
  scale_x_datetime(date_labels = "%m/%d") +
  scale_y_continuous("RWC (g/g)") +
  theme_bw() +
  theme(axis.title.x = element_blank())


