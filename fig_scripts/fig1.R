# First figure
# pull SRM met data for long-term monthly means
# 2023 daily values + soil moisture treatments

library(tidyverse)
library(plantecophys)
library(cowplot)

#### Fig A - SRM ####
srm23 <- read_csv("data/srm/US-SRM_HH_202212312330_202312312330.csv",
                  col_types = "cc") |> 
  select(starts_with("TIMESTAMP"),
         P,
         starts_with("TA_"),
         starts_with("RH_"))
srm_ameriflux <- read_csv("data/srm/AMF_US-SRM_BASE_HH_26-5.csv",
                skip = 2,
                col_types = "cc") |> 
  select(starts_with("TIMESTAMP"),
         P,
         starts_with("TA_"),
         starts_with("RH_"))

# Aggregate from 2004 - 2023 and use height 1 = 2 m above the ground
srm <- srm23 |> 
  bind_rows(srm_ameriflux) |> 
  mutate(dt = as.POSIXct(TIMESTAMP_START, 
                         format = "%Y%m%d%H%M", 
                         tz = "America/Phoenix"),
         date = as.Date(dt, tz = "America/Phoenix")) |> 
  select(dt, date, P, ends_with("1_1")) |> 
  mutate(TA = na_if(TA_1_1_1, -9999),
         RH = na_if(RH_1_1_1, -9999),
         P = na_if(P, -9999)) |> 
  select(-ends_with("1_1"))

# Calculate daily mean, min, max
srm_daily <- srm |> 
  mutate(VPD = plantecophys::RHtoVPD(RH, TA)) |> 
  group_by(date) |> 
  summarize(Tmax = max(TA, na.rm = TRUE),
            Tmin = min(TA, na.rm = TRUE),
            Tmean = mean(TA, na.rm = TRUE),
            Tn = sum(!is.na(TA)),
            Dmax = max(VPD, na.rm = TRUE),
            Dmin = min(VPD, na.rm = TRUE),
            Dmean = mean(VPD, na.rm = TRUE),
            Dn = sum(!is.na(VPD)),
            ppt = sum(P, na.rm = TRUE),
            pn = sum(!is.na(P)))


# Separate out P and T/D 
# Filter only days where 40 or more observations were made
# Calculate monthly sums/means, then take 20 year average
srm_p <- srm_daily |> 
  select(date, ppt, pn) |> 
  filter(pn >= 40) |> 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) |> 
  group_by(year, month) |> 
  summarize(p_month = sum(ppt)) |> 
  group_by(month) |> 
  summarize(m_mon_ppt = mean(p_month),
            sd_mon_ppt = sd(p_month),
            n = n(),
            se_mon_ppt = sd(p_month)/sqrt(n))

srm_td <- srm_daily |> 
  select(date, Tmean, Tmin, Tmax, Tn, Dmean, Dn) |> 
  filter(Tn >= 40) |> 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) |> 
  group_by(year, month) |> 
  summarize(t_month = mean(Tmean),
            tmin_month = mean(Tmin),
            tmax_month = mean(Tmax),
            d_month = mean(Dmean)) |> 
  group_by(month) |> 
  summarize(m_mon_T = mean(t_month),
            m_mon_Tmin = mean(tmin_month),
            m_mon_Tmax = mean(tmax_month),
            n = n(),
            se_mon_T = sd(t_month)/sqrt(n),
            m_mon_D = mean(d_month),
            se_mon_D = sd(d_month)/sqrt(n))

# 20-year mean monthly precip, Tmean (with range from Tmin to Tmax), and VPD
fig_a <- ggplot() +
  geom_col(data = srm_p, 
           aes(x = month, y = m_mon_ppt)) +
  geom_errorbar(data = srm_p,
                aes(x = month,
                    ymin = m_mon_ppt - se_mon_ppt,
                    ymax = m_mon_ppt + se_mon_ppt),
                width = 0) +
  geom_ribbon(data = srm_td,
              aes(x = month, 
                  ymin = m_mon_Tmin, ymax = m_mon_Tmax),
              alpha = 0.5,
              fill = "gray70") +
  geom_line(data = srm_td,
            aes(x = month, y = m_mon_T)) +
  geom_point(data = srm_td,
             aes(x = month, y = m_mon_T)) +
  # geom_errorbar(data = srm_td,
  #               aes(x = month,
  #                   ymin = m_mon_T - se_mon_T,
  #                   ymax = m_mon_T + se_mon_T),
  #               width = 0)
  geom_line(data = srm_td,
            aes(x = month, y = m_mon_D*20 + 15),
            color = "coral") +
  geom_point(data = srm_td,
             aes(x = month, y = m_mon_D*20 + 15),
             color = "coral") +
  scale_x_continuous(breaks = 1:12) +
                     # labels = c("J","F","M", "A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(expression(paste(T[air], " (°C) | precip (mm)")),
                     sec.axis = sec_axis(~(.-15)/20,
                                         "VPD (kPa)")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.right = element_text(color = "coral"))
  
  
fig_a


#### Fig B - RainMan ####

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
               values_to = "irig")

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
fig_b <- env |> 
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
  # geom_ribbon(aes(x = date, ymin = Dmin*12, ymax = Dmax*12),
  #             alpha = 0.5,
  #             fill = "coral") +
  geom_col(data = irig_long |>  filter(trt_s != "S3"),
           aes(x = date,
               y = irig, 
               fill = trt_s),
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

fig_b

# Put it together and save
fig1 <- plot_grid(fig_a, fig_b, align = "v",
          ncol = 1,
          labels = "auto")

ggsave("fig_scripts/fig1.png",
       fig1,
       height = 6,
       width = 8,
       units = "in")
