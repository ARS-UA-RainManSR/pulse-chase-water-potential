# make figures
library(tidyverse)
library(cowplot)

# read in  cleaned datasets
wp_rwc <- read_csv("data_clean/wp_rwc.csv") |> 
  mutate(period = factor(period, levels = c("predawn", "midday")),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")))
wp_wide <- read_csv("data_clean/wp_wide.csv")
vpd <- read_csv("data_clean/vpd_daily_daytime.csv")
vwc <- read_csv("data_clean/vwc_daily_daytime.csv")
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(date >= min(wp_rwc$date_col),
         date <= max(wp_rwc$date_col))

cde <- read_csv("data_clean/cde_daytime_pulse.csv")

#### Figure 1: raw/summarized LWP and VWC ####

irig_plot <- irig |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4"))

vwc_plot <- vwc |> 
  filter(period == "day",
         summer != "S3", 
         depth != "75 cm", 
         date <= as.Date("2023-09-11")) |> 
  rename(trt_s = summer)

vpd_plot <- vpd |> 
  filter(period == "day",
         location == "inside",
         date <= as.Date("2023-09-11")) |> 
  select(date, mean) |> 
  rename(vpd_kpa = mean)

WP_sum <- wp_rwc |> 
  group_by(date_col, pulse_num, trt_s, period2) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))),
            RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC)))) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num))

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])),
          lapply(c("0-12 cm", "25 cm"), function(i) bquote(Theta[.(i)])))

fig1a <- ggplot() +
  geom_vline(data = irig_plot,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = wp_mpa,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_line(data = vwc_plot,
            aes(x = date,
                y = mean*25-6,
                color = depth)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)")),
                     sec.axis = sec_axis(~(.+6)/25,
                                         name = expression(paste(Theta, " (", cm^3, cm^-3, ")")))) +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
                  aes(x = date_col,
                      y = WP_m,
                      color = period2),
                  size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = WP_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  facet_wrap(~trt_s,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "Paired")),
                     breaks = c("PD", "MD", "0-12 cm", "25 cm"),
                     labels = labs) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA))))


# counterpart with RWC and VPD
labs2 <- c(lapply(c("PD", "MD"), function(i) bquote(RWC[.(i)])),
           "VPD")
fig1b <- ggplot() +
  geom_vline(data = irig_plot,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = RWC,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_line(data = vpd_plot,
            aes(x = date,
                y = vpd_kpa/5,
                color = "VPD")) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
                     limits = c(0, 1),
                     sec.axis = sec_axis(~.*5,
                                         name = "VPD (kPa)")) +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = RWC_m - RWC_sd,
                    ymax = RWC_m + RWC_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = RWC_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = RWC_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  facet_wrap(~trt_s,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = rev(RColorBrewer::brewer.pal(5, "Paired")[c(5,3,4)]),
                     breaks = c("PD", "MD", "VPD"),
                     labels = labs2) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.26),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 16, NA))))

fig1 <- plot_grid(fig1a, fig1b, nrow = 2,
                  align = "v",
                  labels = "auto")
fig1
ggsave(filename = "figures/fig1_raw_ts.png",
       plot = fig1,
       height = 5,
       width = 9,
       units = "in")

#### Figure 2: LWP vs. RWC by pulse, show hysteresis ####
vpd_plot <- vpd |> 
  filter(period == "day",
         location == "inside") |> 
  select(date, mean) |> 
  rename(vpd_kpa = mean)


wp_rwc_plot <- wp_rwc |>
  filter(trt_s %in% c("S1", "S2", "S4")) |>  
  # reclassifly pulses as 1 or 2
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor()) |> 
  left_join(vpd_plot, by = join_by("date_col" == "date"))


wp_sum_plot <- wp_rwc_plot |> 
  group_by(date_col, days_since_pulse, pulse_num2, trt_s, period) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))),
            RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC)),
            vpd = mean(vpd_kpa))) 

fig2 <- ggplot() +
  geom_point(data = wp_rwc_plot,
             aes(x = wp_mpa, y = RWC,
                 color = pulse_num2),
             alpha = 0.15) +
  geom_errorbarh(data = wp_sum_plot,
                aes(xmin = WP_m - WP_sd, xmax = WP_m + WP_sd,
                    y = RWC_m,
                    color = pulse_num2), alpha = 0.5) +
  geom_errorbar(data = wp_sum_plot,
                aes(x = WP_m, 
                    ymin = RWC_m - RWC_sd, ymax = RWC_m + RWC_sd,
                    color = pulse_num2), alpha = 0.5) +
  geom_point(data = wp_sum_plot,
             aes(x = WP_m, y = RWC_m,
                 color = pulse_num2),
             size = 2) +
  geom_path(data = wp_sum_plot,
            aes(x = WP_m,
                y = RWC_m,
                linetype = pulse_num2),
            arrow = arrow(type = "closed",
                          length = unit(0.15, "cm")),
            show.legend = FALSE) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period),
             space = "free") +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_x_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual("Pulse",
                     values = c("coral","cornflowerblue")) +
  scale_shape_manual("Pulse", values = 16:15) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.08, 0.9),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(keyheight = 0.75,
                              override.aes = list(linetype = NULL)))


ggsave("figures/fig2_wp_rwc.png",
       plot = fig2,
       height = 4,
       width = 6,
       units = "in")


#### Figure 3: LWP by days since pulse, with CDE ####
# Limit to only S1 and S2
cde_plot <- cde |> 
  filter(pulse_num %in% unique(wp_rwc$pulse_num)) |> 
  # reclassifly pulses as 1 or 2
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor()) |> 
  filter(trt_s != "S4")

wp_sum_plot <- wp_rwc_plot |> 
  group_by(date_col, days_since_pulse, pulse_num2, trt_s, period) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))),
            vpd = mean(vpd_kpa)) 

# need to duplicate day 0 of pulse 2 for S2 as day 7
dup <- wp_sum_plot |> 
  filter(trt_s == "S2",
         days_since_pulse == 0,
         pulse_num2 == "Aug 21") |> 
  mutate(days_since_pulse = 7,
         pulse_num2 = "Aug 14")

wp_sum_plot2 <- wp_sum_plot |> 
  bind_rows(dup) |> 
  filter(trt_s != "S4")
  # not needed if not coloring by CDE
  # left_join(select(cde, date, trt_s, cde),
  #           by = join_by("date_col" == "date", "trt_s"))

fig3 <- ggplot() +
  geom_vline(xintercept = 0.5,
             lty = 2) +
  geom_line(data = wp_sum_plot2,
             aes(x = days_since_pulse,
                 y = WP_m,
                 color = pulse_num2)) +
  geom_errorbar(data = wp_sum_plot2,
             aes(x = days_since_pulse,
                 ymin = WP_m - WP_sd,
                 ymax = WP_m + WP_sd,
                 color = pulse_num2), 
             alpha = 0.5, width = 0.15) +
  geom_point(data = wp_sum_plot2,
             aes(x = days_since_pulse,
                 y = WP_m,
                 color = pulse_num2),
             size = 1.5) +
  geom_line(data = cde_plot,
            aes(x = days_since_pulse,
                y = cde/3 - 5, 
                color = pulse_num2,
                # linetype = pulse_num2
                ),
            linetype = 3) +
   facet_grid(cols = vars(trt_s), 
             rows = vars(period),
             scales = "free_x",
             space = "free_x",
             switch = "y") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)")),
                     sec.axis = sec_axis(~(.+5)*3,
                                         name = "CDE (kPa)") ) +
  scale_x_continuous("Day of pulse",
                     breaks = unique(wp_sum_plot2$days_since_pulse)) +
  scale_color_manual("Pulse",
                     values = c("coral","cornflowerblue")) +
  scale_linetype_manual("Variable", values = c(1,3)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.875, 0.65),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(override.aes = list(linetype = c(3, 3)),
                              keyheight = 0.75))

ggsave("figures/fig3_wp_cde.png",
       plot = fig3,
       height = 3,
       width = 6,
       units = "in")


#### Figure 4: MD vs. PD by pulse, with hysteresis arrows ####

wp_wide_plot <- wp_wide |> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor()) |> 
  left_join(vpd_plot, by = join_by("date_col" == "date"))

wp_wide_sum <- wp_wide_plot |> 
  group_by(date_col, days_since_pulse, pulse_num2, trt_s) |> 
  summarize(pd_mean = mean(predawn,  na.rm = TRUE),
            pd_sd = sd(predawn, na.rm = TRUE),
            md_mean = mean(midday,  na.rm = TRUE),
            md_sd = sd(midday, na.rm = TRUE),
            vpd = unique(vpd_kpa))

# need to duplicate day 0 of pulse 2 for S2 as day 7
dup <- wp_wide_sum |> 
  filter(trt_s == "S2",
         days_since_pulse == 0,
         pulse_num2 == "Aug 21") |> 
  mutate(days_since_pulse = 7,
         pulse_num2 = "Aug 14")

wp_wide_sum2 <- wp_wide_sum |> 
  bind_rows(dup)

  
fig4 <- ggplot() + 
  geom_abline(slope = 1, 
              intercept = 0,
              color = "gray",
              linetype = 1) +
  geom_point(data = wp_wide_plot,
             aes(x = predawn,
                 y = midday,
                 color = pulse_num2),
             alpha = 0.15) +
  geom_errorbar(data = wp_wide_sum2,
                aes(x = pd_mean,
                    ymin = md_mean - md_sd,
                    ymax = md_mean + md_sd,
                    color = pulse_num2),
                alpha = 0.5, width = 0.15) +
  geom_errorbarh(data = wp_wide_sum2,
                aes(y = md_mean,
                    xmin = pd_mean - pd_sd,
                    xmax = pd_mean + pd_sd,
                    color = pulse_num2),
                alpha = 0.5, height = 0.15) +
  geom_point(data = wp_wide_sum2,
             aes(x = pd_mean,
                 y = md_mean,
                 color = pulse_num2),
             size = 2) +
  geom_path(data = wp_wide_sum2,
            aes(x = pd_mean,
                y = md_mean,
                # color = pulse_num2,
                linetype = pulse_num2),
            arrow = arrow(type = "closed",
                          length = unit(0.15, "cm")),
            show.legend = FALSE) +
  facet_wrap(vars(trt_s)) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste(Psi[MD], " (MPa)"))) +
  scale_color_manual("Pulse",
                     values = c("coral","cornflowerblue")) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.92, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(keyheight = 0.75,
                              override.aes = list(linetype = NULL)))

ggsave("figures/fig4_md_pd.png",
       plot = fig4,
       height = 3,
       width = 6,
       units = "in")

###### Sandbox figures ######

#### LWP by days since pulse, with CDE ####
wp_sum_plot <- wp_rwc_plot |> 
  group_by(date_col, days_since_pulse, pulse_num2, trt_s, period) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))),
            vpd = mean(vpd_kpa)) 

# need to duplicate day 0 of pulse 2 for S2 as day 7
dup <- wp_sum_plot |> 
  filter(trt_s == "S2",
         days_since_pulse == 0,
         pulse_num2 == "Aug 21") |> 
  mutate(days_since_pulse = 7,
         pulse_num2 = "Aug 14")

wp_sum_plot2 <- wp_sum_plot |> 
  bind_rows(dup) |> 
  filter(trt_s != "S4")

# combind vwc_plot and cde

vwc_cde <- vwc_plot |> 
  left_join(cde, by = join_by("date", "trt_s")) |> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor()) |> 
  filter(!is.na(pulse_num2)) |> 
  rename("interval" = "period")

ggplot() +
  geom_vline(xintercept = 0.5,
             # color = "gray",
             lty = 2) +
  geom_line(data = wp_sum_plot2,
            aes(x = days_since_pulse,
                y = WP_m,
                color = pulse_num2)) +
  geom_errorbar(data = wp_sum_plot2,
                aes(x = days_since_pulse,
                    ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd,
                    color = pulse_num2), 
                alpha = 0.5, width = 0.15) +
  geom_point(data = wp_sum_plot2,
             aes(x = days_since_pulse,
                 y = WP_m,
                 color = pulse_num2),
             size = 1.5) +
  geom_line(data = vwc_cde |>  filter(depth == "0-12 cm", trt_s != "S4"),
            aes(x = days_since_pulse,
                y = mean*25 - 6,
                color = pulse_num2),
            linetype = 1) +
  facet_grid(cols = vars(trt_s),
             rows = vars(period),
             scales = "free_x",
             space = "free_x",
             switch = "y") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)")),
                     sec.axis = sec_axis(~(.+6)/25,
                                         name = expression(paste(Theta[0-12], " (", cm^3, cm^-3, ")"))) ) +
  scale_x_continuous("Day of pulse",
                     breaks = unique(wp_sum_plot2$days_since_pulse)) +
  scale_color_manual("Pulse",
                     values = c("coral","cornflowerblue")) +
  scale_linetype_manual("Variable", values = c(1, 3)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.875, 0.65),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1)),
                              keyheight = 0.75))

##### LWP timeseries by plot for S4 #####

wp_ts_plot <- wp_wide |>
  # reclassifly pulses as 1 or 2
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor(),
         plot2 = paste0("H", house, "P", plot)) |> 
  left_join(vpd_plot, by = join_by("date_col" == "date"))


ggplot() +
  geom_point(data = wp_ts_plot,
             aes(x = date_col, y = predawn,
                 color = "predawn")) +
  geom_line(data = wp_ts_plot,
             aes(x = date_col, y = predawn,
                 color = "predawn")) +
  geom_point(data = wp_ts_plot,
             aes(x = date_col, y = midday,
                 color = "midday")) +
  geom_line(data = wp_ts_plot,
            aes(x = date_col, y = midday,
                color = "midday")) +
  facet_wrap(vars(trt_s, plot2), nrow = 3) 

#### RWC timeseries by plot ####
wp_rwc_plot <- wp_rwc |>
  filter(trt_s %in% c("S1", "S2", "S4")) |>  
  # reclassifly pulses as 1 or 2
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor(),
         plot2 = paste0("H", house, "P", plot)) |> 
  left_join(vpd_plot, by = join_by("date_col" == "date"))

ggplot() +
  geom_point(data = wp_rwc_plot,
             aes(x = date_col, y = RWC,
                 color = period)) +
  geom_line(data = wp_rwc_plot,
            aes(x = date_col, y = RWC,
                color = period)) +
  facet_wrap(vars(trt_s, plot2), nrow = 3) 
