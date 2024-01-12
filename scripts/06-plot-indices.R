# compare indices and plot w/ WP
library(tidyverse)
library(cowplot)
library(ggpubr)

# load data files
# indices from hyperspectral
hyp_ind <- read_csv("data/hyperspec_1/Indices_Rep1_merged.csv") |> 
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         house = str_extract(House, "\\d") |> as.numeric(),
         plot = str_extract(Plot, "\\d{1,2}") |> as.numeric()) |> 
  # remove raw WP and RWC
  select(-WP, -RWC)

# WP and RWC cleaned by RWC, join with hyp_ind
wp_rwc <- read_csv("data_clean/wp_rwc.csv") |> 
  mutate(period = factor(period, levels = c("predawn", "midday")),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |> 
  left_join(hyp_ind, by = join_by("date_col" == "Date",
                                  "period" == "Time",
                                  "house", "plot",
                                  "trt" == "Treat"))

# WP only cleaned
wp_wide <- read_csv("data_clean/wp_wide.csv")

# irrigation
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4"))

#### Fig exploration of NDWI ####

WP_sum <- wp_rwc |> 
  group_by(date_col, pulse_num, trt_s, period2) |> 
  summarize(WP_m = mean(wp_mpa, na.rm = TRUE),
            WP_sd = sd(wp_mpa, na.rm = TRUE),
            WP_n = sum(length(!is.na(wp_mpa))),
            RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC))),
            NDVI_m = mean(NDVI, na.rm = TRUE),
            NDVI_sd = sd(NDVI, na.rm = TRUE),
            NDVI_n = sum(length(!is.na(NDVI))),
            NDWI1_m = mean(NDWI1, na.rm = TRUE),
            NDWI1_sd = sd(NDWI1, na.rm = TRUE),
            NDWI1_n = sum(length(!is.na(NDWI1))),
            NDWI2_m = mean(NDWI2, na.rm = TRUE),
            NDWI2_sd = sd(NDWI2, na.rm = TRUE),
            NDWI2_n = sum(length(!is.na(NDWI2))),
            NDWI3_m = mean(NDWI3, na.rm = TRUE),
            NDWI3_sd = sd(NDWI3, na.rm = TRUE),
            NDWI3_n = sum(length(!is.na(NDWI3))),
            PRI_m = mean(PRI, na.rm = TRUE),
            PRI_sd = sd(PRI, na.rm = TRUE),
            PRI_n = sum(length(!is.na(PRI)))) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num))

# NDVI
fig5a <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = NDVI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = NDVI_m - NDVI_sd,
                    ymax = NDVI_m + NDVI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = NDVI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = NDVI_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~trt_s,
             nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))
  # guides(color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA))))

# NDWI1
fig5b <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = NDWI1,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = NDWI1_m - NDWI1_sd,
                    ymax = NDWI1_m + NDWI1_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = NDWI1_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = NDWI1_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~trt_s,
             nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# NDWI2
fig5c <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = NDWI2,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = NDWI2_m - NDWI2_sd,
                    ymax = NDWI2_m + NDWI2_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = NDWI2_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = NDWI2_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~trt_s,
             nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# NDWI3
fig5d <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = NDWI3,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = NDWI3_m - NDWI3_sd,
                    ymax = NDWI3_m + NDWI3_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = NDWI3_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = NDWI3_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~trt_s,
             nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# PRI
fig5e <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = 2)  +
  geom_point(data = wp_rwc,
             aes(x = date_col,
                 y = PRI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = WP_sum,
                aes(x = date_col,
                    ymin = PRI_m - PRI_sd,
                    ymax = PRI_m + PRI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = WP_sum,
             aes(x = date_col,
                 y = PRI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = WP_sum,
            aes(x = date_col,
                y = PRI_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~trt_s,
             nrow = 1) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.27, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))
# guides(color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA))))

Fig_NDWI <- plot_grid(fig5b, fig5c, fig5d,
          nrow = 3)

ggsave("figures/Fig_NDWI_comp.png",
       plot = Fig_NDWI,
       height = 5, 
       width = 7, 
       units = "in")

#### Fig 5 facet_grid plot with NDVI, PRI, and NDWI ####

hyp_long <- wp_rwc |> 
  select(1:15, NDVI, PRI, NDWI2) |> 
  rename(NDWI = NDWI2) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),) |> 
  pivot_longer(cols = c("NDVI", "PRI", "NDWI"),
               names_to = "index",
               values_to = "value") |> 
  mutate(index = factor(index, levels = c("NDVI", "PRI", "NDWI")))

hyp_long_sum  <- hyp_long |> 
  group_by(date_col, pulse_num, pulse_num2, trt_s, period2, index, ) |> 
  summarize(ind_m = mean(value, na.rm = TRUE),
            ind_sd = sd(value, na.rm = TRUE))

fig5 <- ggplot() +
  geom_vline(data = irig,
                     aes(xintercept = date),
                     lty = 2)  +
  geom_point(data = hyp_long,
             aes(x = date_col,
                 y = value,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_long_sum,
                aes(x = date_col,
                    ymin = ind_m - ind_sd,
                    ymax = ind_m + ind_sd,
                    color = period2),
                alpha = 0.5, width = 0.25) +
  geom_point(data = hyp_long_sum,
             aes(x = date_col,
                 y = ind_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_long_sum,
            aes(x = date_col,
                y = ind_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  facet_grid(cols = vars(trt_s),
             rows = vars(index),
             scales = "free",
             space = "free_x",
             switch = "y") +
  scale_x_date(date_labels = "%b %d", 
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"),
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +

  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.95, 0.73),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

ggsave(filename = "figures/fig5_hyp_ts.png",
       plot = fig5,
       height = 7,
       width = 9,
       units = "in")

#### Fig 6 comparison of indices + WP ####

wp_rwc_plot <- wp_rwc |> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(3, 7, 13) ~ "Aug 14",
                                pulse_num %in% c(8, 15) ~ "Aug 21") |> 
           as.factor(),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) 

fig6a <- ggplot(wp_rwc_plot, aes(x =  NDVI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

fig6b <- ggplot(wp_rwc_plot, aes(x =  PRI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

fig6c <- ggplot(wp_rwc_plot, aes(x =  NDWI2, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_x_continuous("NDWI") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

plot_grid(fig6a, fig6b, fig6c,
          nrow = 3)

#### Fig 6 comparison of indices + WP ####


fig7a <- ggplot(wp_rwc_plot, aes(x =  NDVI, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

fig7b <- ggplot(wp_rwc_plot, aes(x =  PRI, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

fig7c <- ggplot(wp_rwc_plot, aes(x =  NDWI2, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_x_continuous("NDWI") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

plot_grid(fig7a, fig7b, fig7c,
          nrow = 3)
####
# all significant with WP
summary(lm(wp_mpa ~ NDVI, data = wp_rwc_plot)) #R2 = 0.166
summary(lm(wp_mpa ~ PRI, data = wp_rwc_plot)) #R2 = 0.183
summary(lm(wp_mpa ~ NDWI2, data = wp_rwc_plot)) #R2 = 0.018

# all significant with RWC
summary(lm(RWC ~ NDVI, data = wp_rwc_plot)) #R2 = 0.165
summary(lm(RWC ~ PRI, data = wp_rwc_plot)) #R2 = 0.124
summary(lm(RWC ~ NDWI2, data = wp_rwc_plot)) #R2 = 0.074


ggplot(wp_rwc_plot, aes(x =  NDVI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  PRI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  NDWI2, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 


ggplot(wp_rwc_plot, aes(x =  NDVI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  PRI, y = wp_mpa)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  NDVI, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  PRI, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 

ggplot(wp_rwc_plot, aes(x =  NDWI2, y = RWC)) +
  geom_point(aes(color = period2)) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.title = element_blank(),
        legend.position = c(0.575, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) 


ggplot(wp_rwc, aes(x =  RWC, y = wp_mpa)) +
  geom_point(aes(color = as.factor(pulse_num))) +
  geom_smooth(method = lm, linetype = "dashed", color = "black", se = FALSE) +
  stat_cor(aes(label = after_stat(rr.label)), color = "black", geom = "label") +
  facet_grid(cols = vars(trt_s), scales = "free")

