# First figure
# Show climate first
# Then treatments
# Then pulse chase
# Rename pulses to P3.5, P7, and P 21?

library(tidyverse)
library(cowplot)



#### Fig 1 - just climate ####

# Read in wide irrigation data for entire summer 2023
irig <- read_csv("data/irrig.csv",
                 locale = locale(tz = "America/Phoenix")) |> 
  mutate(date = as.Date(lubridate::mdy(date, tz = "America/Phoenix")))
str(irig)

# Make long
irig_long <- data.frame(date = seq(as.Date("2023-07-03", tz = "America/Phoenix"),
                                   as.Date("2023-09-25", tz = "America/Phoenix"),
                                   by = 1)) |> 
  left_join(irig, by = join_by("date")) |> 
  replace_na(list(S1 = 0,
                  S2 = 0,
                  S3 = 0,
                  S4 = 0)) |> 
  pivot_longer(starts_with("S"),
               names_to = "trt_s",
               values_to = "irig") |>
  filter(trt_s != "S3") |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Set start and end points of the summer period
summer_st <- min(irig_long$date)
summer_en <- max(irig_long$date)

# Calcluate daily mean/max/min temp and vpd
# From on-site, outside half-hourly data
env <- read_csv("data/env/Halfhourly_VWC_SWP_Weather_092323.csv",
                locale = locale(tz = "America/Phoenix")) |> 
  mutate(date = as.Date(TIMESTAMP, tz = "America/Phoenix")) |> 
  filter(date >= summer_st,
         date <= summer_en) |> 
  select(TIMESTAMP, date, T_outside, RH_outside) |> 
  mutate(VPD = plantecophys::RHtoVPD(RH_outside, T_outside)) |> 
  group_by(date) |> 
  summarize(Tmax = max(T_outside, na.rm = TRUE),
            Tmin = min(T_outside, na.rm = TRUE),
            Tmean = mean(T_outside, na.rm = TRUE),
            Dmax = max(VPD, na.rm = TRUE),
            Dmin = min(VPD, na.rm = TRUE),
            Dmean = mean(VPD, na.rm = TRUE))

# Create start/end dates for each of the 5 pulses
pulse <- data.frame(trt_s = c("S4", "S1", "S2", "S1", "S2"),
                    pulse_num = c(1, 1, 1, 2, 2),
                    st = c(as.Date("2023-08-14", tz = "America/Phoenix"),
                           as.Date("2023-08-14", tz = "America/Phoenix"),
                           as.Date("2023-08-14", tz = "America/Phoenix"),
                           as.Date("2023-08-21", tz = "America/Phoenix"),
                           as.Date("2023-08-21", tz = "America/Phoenix")),
                    en = c(as.Date("2023-09-04", tz = "America/Phoenix"),
                           as.Date("2023-08-17", tz = "America/Phoenix"),
                           as.Date("2023-08-21", tz = "America/Phoenix"),
                           as.Date("2023-08-24", tz = "America/Phoenix"),
                           as.Date("2023-08-28", tz = "America/Phoenix")))

# Obtain actual sampling dates
wp_dates <- read_csv("data_clean/wp_wide.csv") |> 
  group_by(date_col) |> 
  count()

# Summer (JAS) 2023 irrigation, daily Tmean (with range from Tmin to Tmax), and daily VPD
# With rectangles indicating pulses
# and x's marking sampling days
env |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)"),
                     limits = c(0, 62)) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1a.png",
       height = 3,
       width = 8,
       units = "in")


### with S1

env |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  geom_col(data = irig_long |> filter(trt_label == "P3.5"),
           aes(x = date,
               y = irig,
               fill = trt_label),
           position = "dodge") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C) | irrigation (mm)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)"),
                     limits = c(0, 62)) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1b.png",
       height = 3,
       width = 8,
       units = "in")


# With S1 and S2

env |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  geom_col(data = irig_long |> filter(trt_label != "P21"),
           aes(x = date,
               y = irig,
               fill = trt_label),
           position = "dodge") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C) | irrigation (mm)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)"),
                     limits = c(0, 62)) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1c.png",
       height = 3,
       width = 8,
       units = "in")


# With all 3
env |> 
  ggplot() +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  geom_col(data = irig_long,
           aes(x = date,
               y = irig,
               fill = trt_label),
           position = "dodge") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C) | irrigation (mm)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)"),
                     limits = c(0, 62)) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1d.png",
       height = 3,
       width = 8,
       units = "in")


# With pulse shading
env |> 
  ggplot() +
  geom_rect(data = pulse, 
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1) +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  geom_col(data = irig_long,
           aes(x = date,
               y = irig, 
               fill = trt_label),
           position = "dodge") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C) | irrigation (mm)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)")) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1e.png",
       height = 3,
       width = 8,
       units = "in")


# With days of sampling
env |> 
  ggplot() +
  geom_rect(data = pulse, 
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1) +
  geom_point(data = wp_dates,
             aes(x = date_col, y = 60),
             shape = 4, size = 1.5) +
  geom_ribbon(aes(x = date, ymin = Tmin, ymax = Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(aes(x = date, y = Tmean)) +
  geom_line(aes(x = date, y = Dmean*12),
            color = "coral") +
  geom_col(data = irig_long,
           aes(x = date,
               y = irig, 
               fill = trt_label),
           position = "dodge") +
  scale_x_date(breaks = as.Date(c("2023-07-03", "2023-07-24",
                                  "2023-08-14", "2023-09-04",
                                  "2023-09-25")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(T[air], " (°C) | irrigation (mm)")),
                     sec.axis = sec_axis(~./12,
                                         "VPD (kPa)")) +
  scale_fill_brewer(palette = "Blues") +
  labs(fill = "Treatment") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"),
        legend.position = c(0.93, 0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.background = element_blank())

ggsave("fig_scripts/egu_talk/fig1f.png",
       height = 3,
       width = 8,
       units = "in")
