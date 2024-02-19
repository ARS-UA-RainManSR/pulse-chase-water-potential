# Create new figures such as bivariate plots and stacked pulse figures

library(tidyverse)
library(cowplot)

#  read in hyperspec indices
hyp_ind <- read_csv("data/hyperspec_1/Indices_Rep1_merged.csv") |> 
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         house = str_extract(House, "\\d") |> as.numeric(),
         plot = str_extract(Plot, "\\d{1,2}") |> as.numeric(),
         period = factor(Time, levels = c("predawn", "midday"))) |> 
  # remove raw WP and RWC
  select(-WP, -RWC) 

# read in cleaned long data (includes all cleaned wp and rwc)
dat_long <- read_csv("data_clean/wp_rwc_long.csv")

# read in irrigation data
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(date >= min(dat_long$date_col),
         date <= max(dat_long$date_col))
irig_plot <- irig |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4"))

# read in summarized vpd (PD and MD inside)
vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "inside",
         period %in% c("PD", "MD"),
         !is.na(mean)) |>  
  select(-location, -sd) |> 
  mutate(period = case_when(period == "PD" ~ "predawn",
                            period == "MD" ~ "midday")) |> 
  rename("VPD" = "mean")

# read in summarized vwc (morning, all depths)
vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  rename("interval" = "period", "trt_s" = "summer") |> 
  filter(interval == "morn") |> 
  mutate(depth2 = case_when(depth == "0-12 cm" ~ "VWC_1",
                            depth == "25 cm" ~ "VWC_2",
                            depth == "75 cm" ~ "VWC_3")) |> 
  select(-sd, -depth, -interval) |> 
  pivot_wider(names_from = depth2,
              values_from = mean)

swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  rename("interval" = "period", "trt_s" = "summer") |> 
  filter(interval == "morn") |> 
  mutate(depth2 = case_when(depth == "0-12 cm" ~ "SWP_1",
                            depth == "25 cm" ~ "SWP_2",
                            depth == "75 cm" ~ "SWP_3")) |> 
  select(-sd, -depth, -interval) |> 
  pivot_wider(names_from = depth2,
              values_from = mean)


# join together for plotting
dat_all <- dat_long |> 
  left_join(vpd, by = join_by("date_col" == "date", "period")) |> 
  left_join(vwc, by = join_by("date_col" == "date", "trt_s")) |> 
  left_join(swp, by = join_by("date_col" == "date", "trt_s")) |> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor(),
         period = factor(period, levels = c("predawn", "midday")))


# Plot against VPD
dat_all |> 
  filter(days_since_pulse != 0) |> 
  ggplot(aes(x = VPD)) +
  geom_point(aes(y = value, color = period)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(variable),
             scales = "free_y",
             switch = "y") +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.background.y = element_blank())

# Plot against VWC
dat_all |> 
  filter(days_since_pulse != 0) |> 
  ggplot(aes(x = VWC_1)) +
  geom_point(aes(y = value, color = period)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(variable),
             scales = "free",
             space = "free_x",
             switch = "y") +
  scale_x_continuous("VWC shallow",
                     breaks = seq(0, 0.125, by = 0.025)) +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.background.y = element_blank())

# Plot against SWP
dat_all |> 
  filter(days_since_pulse != 0) |> 
  ggplot(aes(x = SWP_1)) +
  geom_point(aes(y = value, color = period)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(variable),
             scales = "free",
             space = "free_x",
             switch = "y") +
  scale_x_continuous("SWP shallow",
                     breaks = seq(-2.5, 0, by = 0.5)) +
  theme_bw(base_size = 14) +
  theme(axis.title.y = element_blank(),
        strip.placement = "outside",
        strip.background.y = element_blank())

# Test predawn WP vs SWP for all 3 trts
dat_all |> 
  filter(period == "predawn", 
         variable == "WP") |> 
  ggplot(aes(x = SWP_1,
             y = value)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(aes(color = trt_s)) +
  # geom_smooth(method = "lm") +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)")),
                     limits = c(-4.5, 0)) +
  scale_y_continuous(expression(paste(Psi["leaf, PD"], " (MPa)")),
                     limits = c(-4.5, 0)) +
  coord_equal() +
  theme_bw(base_size = 14)

# Create summary and maybe link with arrows?
dat_pd_sum <- dat_all |> 
  filter(period == "predawn", 
         variable == "WP") |> 
  group_by(trt_s, days_since_pulse, pulse_num2) |> 
  summarize(LWP_m = mean(value, na.rm = TRUE),
            LWP_sd = sd(value, na.rm = TRUE),
            SWP_1 = mean(SWP_1))

# Plot by pulse and trt with arrows
ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(data = filter(dat_all,
                           period == "predawn", 
                           variable == "WP"),
             aes(x = SWP_1,
                 y = value,
                 color = trt_s),
             alpha = 0.25) +
  geom_pointrange(data = dat_pd_sum,
                  aes(x = SWP_1, y = LWP_m,
                      ymin = LWP_m - LWP_sd,
                      ymax = LWP_m + LWP_sd,
                      color = trt_s)) +
  geom_path(data = dat_pd_sum,
            aes(x = SWP_1, y = LWP_m),
            arrow = arrow(type = "closed",
                          length = unit(0.15, "cm")),
            show.legend = FALSE) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)")),
                     limits = c(-4.5, 0)) +
  scale_y_continuous(expression(paste(Psi["leaf, PD"], " (MPa)")),
                     limits = c(-4.5, 0)) +
  facet_grid(cols = vars(trt_s),
             rows = vars(pulse_num2)) +
  coord_equal() +
  theme_bw(base_size = 14)


#### double axis plot for SWP and VWC ####
labs <- c(lapply(c("0-12 cm", "25 cm"), function(i) bquote(Theta[.(i)])),
          lapply(c("0-12 cm", "25 cm"), function(i) bquote(Psi[.(paste("soil,", i))])))
labs
dat_all |> 
  ggplot(aes(x = date_col)) +
  geom_line(aes(y = VWC_1, color = "VWC_1")) +
  geom_line(aes(y = VWC_2, color = "VWC_2")) +
  geom_line(aes(y = SWP_1/10, color = "SWP_1")) +
  geom_line(aes(y = SWP_2/10, color = "SWP_2")) +
  scale_y_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")),
                     sec.axis = sec_axis(~(.-0)*10, 
                                         name = expression(paste(Psi[soil], " (MPa)")))) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")),
                     breaks = c("VWC_1", "VWC_2", "SWP_1", "SWP_2"),
                     labels = labs) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%m-%d") +
  facet_grid(cols = vars(trt_s), 
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank())

# Plot LWP and SWP

lwp_sum <- dat_all |> 
  filter(variable == "WP") |> 
  group_by(date_col, trt_s, period) |> 
  summarize(wp_m = mean(value, na.rm = TRUE),
            wp_sd = sd(value,  na.rm = TRUE))

labs <- c(lapply(c("0-12 cm", "25 cm"), function(i) bquote(Psi[.(paste("soil,", i))])),
          lapply(c("leaf"), function(i) bquote(Psi[.(i)])))
labs
  
dat_all |> 
  filter(variable == "WP") |> 
  mutate(period = factor(period, levels = c("predawn", "midday"))) |> 
  ggplot(aes(x = date_col)) +
  geom_vline(data = irig_plot,
             aes(xintercept = date),
             lty = 2)  +
  geom_line(aes(y = SWP_1, color = "SWP_1")) +
  geom_line(aes(y = SWP_2, color = "SWP_2")) +
  geom_point(aes(y = value, color = "LWP"),
             alpha = 0.25) +
  geom_point(data = lwp_sum,
             aes(y = wp_m, color = "LWP")) +
  geom_errorbar(data = lwp_sum,
                aes(ymin = wp_m - wp_sd,
                    ymax = wp_m + wp_sd, color = "LWP"),
                width = 0) +
  geom_line(data = lwp_sum,
            aes(y = wp_m, color = "LWP")) + 
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(3, "Dark2")),
                     breaks = c("SWP_1", "SWP_2", "LWP"),
                     labels = labs) +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%m-%d") +
  facet_grid(cols = vars(trt_s), 
             rows = vars(period),
             scales = "free",
             space = "free_x") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank())


# Stack and plot for trt_s == S4

# A) SWP
figa <- dat_all |> 
  filter(trt_s == "S4") |> 
  ggplot(aes(x = date_col)) + 
  geom_point(aes(y = SWP_1, col = "SWP_1")) +
  geom_point(aes(y = SWP_2, col = "SWP_2")) +
  geom_line(aes(y = SWP_1, col = "SWP_1")) +
  geom_line(aes(y = SWP_2, col = "SWP_2")) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[1:2]),
                     breaks = c("SWP_1", "SWP_2"),
                     labels = c("0-12 cm", "25 cm")) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))

# A) VWC
figa2 <- dat_all |> 
  filter(trt_s == "S4") |> 
  ggplot(aes(x = date_col)) + 
  geom_point(aes(y = VWC_1, col = "VWC_1")) +
  geom_point(aes(y = VWC_2, col = "VWC_2")) +
  geom_line(aes(y = VWC_1, col = "VWC_1")) +
  geom_line(aes(y = VWC_2, col = "VWC_2")) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[1:2]),
                     breaks = c("VWC_1", "VWC_2"),
                     labels = c("0-12 cm", "25 cm")) +
  scale_y_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))

# B) LWP
lwp_sum_S4 <- dat_all |> 
  filter(variable == "WP",
         trt_s == "S4") |> 
  group_by(date_col, period) |> 
  summarize(wp_m = mean(value, na.rm = TRUE),
            wp_sd = sd(value,  na.rm = TRUE))


figb <- dat_all |> 
  filter(variable == "WP",
         trt_s == "S4") |> 
  ggplot(aes(x = date_col)) +
  geom_point(aes(y = value, col = period),
             alpha = 0.25) +
  geom_errorbar(data = lwp_sum_S4,
                aes(ymin = wp_m - wp_sd,
                    ymax = wp_m + wp_sd, color = period),
                width = 0, alpha = 0.5) +
  geom_point(data = lwp_sum_S4,
             aes(y = wp_m, color = period)) +
  geom_line(data = lwp_sum_S4,
            aes(y = wp_m, color = period)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[3:4]),
                     labels = c("PD", "MD")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))


# C) RWC
rwc_sum_S4 <- dat_all |> 
  filter(variable == "RWC",
         trt_s == "S4") |> 
  group_by(date_col, period) |> 
  summarize(rwc_m = mean(value, na.rm = TRUE),
            rwc_sd = sd(value,  na.rm = TRUE))
figc <- dat_all |> 
  filter(variable == "RWC",
         trt_s == "S4") |> 
  ggplot(aes(x = date_col)) +
  geom_point(aes(y = value, col = period),
             alpha = 0.25) +
  geom_errorbar(data = rwc_sum_S4,
                aes(ymin = rwc_m - rwc_sd,
                    ymax = rwc_m + rwc_sd, color = period),
                width = 0, alpha = 0.5) +
  geom_point(data = rwc_sum_S4,
             aes(y = rwc_m, color = period)) +
  geom_line(data = rwc_sum_S4,
            aes(y = rwc_m, color = period)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[3:4]),
                     labels = c("PD", "MD")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))

# D) NDVI
ndvi_sum_S4 <- hyp_ind |> 
  filter(Summer == "S4") |> 
  group_by(Date, period) |> 
  summarize(ndvi_m = mean(NDVI, na.rm = TRUE),
            ndvi_sd = sd(NDVI,  na.rm = TRUE))

figd <- hyp_ind |> 
  filter(Summer == "S4") |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = NDVI, col = Time),
             alpha = 0.25) +
  geom_errorbar(data = ndvi_sum_S4,
                aes(ymin = ndvi_m - ndvi_sd,
                    ymax = ndvi_m + ndvi_sd, color = period),
                width = 0, alpha = 0.5) +
  geom_point(data = ndvi_sum_S4,
             aes(y = ndvi_m, color = period)) +
  geom_line(data = ndvi_sum_S4,
            aes(y = ndvi_m, color = period)) +
  scale_y_continuous("NDVI") +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[3:4]),
                     labels = c("PD", "MD")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))


# E) PRI
pri_sum_S4 <- hyp_ind |> 
  filter(Summer == "S4") |> 
  group_by(Date, period) |> 
  summarize(pri_m = mean(PRI, na.rm = TRUE),
            pri_sd = sd(PRI,  na.rm = TRUE))
fige <- hyp_ind |> 
  filter(Summer == "S4") |> 
  ggplot(aes(x = Date)) +
  geom_point(aes(y = PRI, col = Time),
             alpha = 0.25) +
  geom_errorbar(data = pri_sum_S4,
                aes(ymin = pri_m - pri_sd,
                    ymax = pri_m + pri_sd, color = period),
                width = 0, alpha = 0.5) +
  geom_point(data = pri_sum_S4,
             aes(y = pri_m, color = period)) +
  geom_line(data = pri_sum_S4,
            aes(y = pri_m, color = period)) +
  scale_y_continuous("PRI",
                     breaks = seq(-0.02, 0.06, by = 0.02)) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")[3:4]),
                     labels = c("PD", "MD")) +
  theme_bw(base_size = 12) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.28, 0.25),
        legend.text = element_text(size = 8, hjust = 0),
        legend.background = element_rect(fill = "transparent"))


fig_stack <- plot_grid(figa, figb, figc, figd, fige,
          ncol = 1,
          align = "v",
          labels = "auto")
ggsave(filename = "figures/fig6_stack_S4.png",
       fig_stack,
       height = 8,
       width = 3.5,
       units = "in")

fig_stack2 <- plot_grid(figa, figa2, figb, figc, figd, fige,
                       ncol = 2,
                       align = "v",
                       labels = "auto")
ggsave(filename = "figures/fig6_stack_S4_wide.png",
       fig_stack2,
       height = 6,
       width = 7, 
       units = "in")
