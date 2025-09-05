# 3 rows (variables) by 3 columns (treatments)
# timeseries of raw data + daily means +/- SD 
# Add SWP to LWP 
# standalone rows of RWC and RWCind

library(tidyverse)
library(RColorBrewer)
library(cowplot)

# Read in long data with indices
all <- read_csv("data_clean/spectra_ind_wp_rwc.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")),
         trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))


# Summarize the 3 variables
all_sum <- all |> 
  group_by(date_col, pulse_num, trt_s, trt_label, period2) |> 
  summarize(WP_m = mean(WP, na.rm = TRUE),
            WP_sd = sd(WP, na.rm = TRUE),
            WP_n = sum(length(!is.na(WP))),
            RWC_m = mean(RWC, na.rm = TRUE),
            RWC_sd = sd(RWC, na.rm = TRUE),
            RWC_n = sum(length(!is.na(RWC))),
            RWCind_m = mean(RWC_ind, na.rm = TRUE),
            RWCind_sd = sd(RWC_ind, na.rm = TRUE),
            RWCind_n = sum(length(!is.na(RWC_ind)))
            ) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2))


# Irrigation data to place vertical lines
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4")) |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Soil water potential
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(all$date_col),
         date <= max(all$date_col),
         depth != "75 cm",
         summer != "S3") |> 
  rename(trt_s = summer) |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Volumetric water content
vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(all$date_col),
         date <= max(all$date_col),
         depth != "75 cm",
         summer != "S3") |> 
  rename(trt_s = summer) |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

####  A) LWP + SWP ####

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])),
          lapply(c("0-10 cm", "25 cm"), function(i) bquote(Psi[.(i)])))

cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")

figa <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = all,
             aes(x = date_col,
                 y = WP,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = all_sum,
                aes(x = date_col,
                    ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = all_sum,
             aes(x = date_col,
                 y = WP_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = all_sum,
            aes(x = date_col,
                y = WP_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  geom_line(data = swp,
            aes(x = date,
                y = mean,
                color = depth,
                lty = depth)) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)")),
                     sec.axis = sec_axis(~.,
                                         name = expression(paste(Psi[soil], " (MPa)")))) +
  facet_wrap(~trt_label,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-03"), 
                            by = 7)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(4, "Paired"))[1:2], cols_br_gn[1], cols_br_gn[1]),
                     breaks = c("PD", "MD", "0-10 cm", "25 cm"),
                     labels = labs) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.3),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA)) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA),
                                                  linetype = c(1, 1, 1, 2)),
                              keyheight = 0.03,
                              default.unit = "cm"))
figa
#### B) RWC + VWC ####
labs2 <- c(lapply(c("PD", "MD"), function(i) bquote(RWC[.(i)])),
           lapply(c("0-10 cm", "25 cm"), function(i) bquote(Theta[.(i)])))


cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")


figb <-  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = all,
             aes(x = date_col,
                 y = RWC,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = all_sum,
                aes(x = date_col,
                    ymin = RWC_m - RWC_sd,
                    ymax = RWC_m + RWC_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = all_sum,
             aes(x = date_col,
                 y = RWC_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = all_sum,
            aes(x = date_col,
                y = RWC_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  geom_line(data = vwc,
            aes(x = date,
                y = mean*6,
                color = depth,
                lty = depth)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
                     # limits = c(0, 1),
                     sec.axis = sec_axis(~./6,
                                         name = expression(paste(Theta, " (", cm^3, cm^-3, ")")))) +  
    facet_wrap(~trt_label,
               nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-03"), 
                            by = 7)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(4, "Paired"))[1:2], cols_br_gn[2], cols_br_gn[2]), 
                     breaks = c("PD", "MD", "0-10 cm", "25 cm"),
                     labels = labs2) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.5),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA)) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA),
                                                  linetype = c(1, 1, 1, 2)),
                              keyheight = 0.03,
                              default.unit = "cm"))

figb

#### C) RWC_ind only ####
labs3 <- c(lapply(c("ind_PD", "ind_MD"), function(i) bquote(RWC[.(i)])))
           # lapply(c("0-10 cm", "25 cm"), function(i) bquote(Theta[.(i)])))


cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")


figc <-  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = all,
             aes(x = date_col,
                 y = RWC_ind,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = all_sum,
                aes(x = date_col,
                    ymin = RWCind_m - RWCind_sd,
                    ymax = RWCind_m + RWCind_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = all_sum,
             aes(x = date_col,
                 y = RWCind_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = all_sum,
            aes(x = date_col,
                y = RWCind_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  # geom_line(data = vwc,
  #           aes(x = date,
  #               y = mean*5,
  #               color = depth,
  #               lty = depth)) +
  # scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
  #                    limits = c(0, 1),
  #                    sec.axis = sec_axis(~./5,
  #                                        name = expression(paste(Theta, " (", cm^3, cm^-3, ")")))) +  
  scale_y_reverse(expression(paste(RWC[ind]))) +
  facet_wrap(~trt_label,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-03"), 
                            by = 7)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(4, "Paired"))[1:2]), 
                     breaks = c("PD", "MD"),
                     labels = labs3) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.24, 0.25),
        legend.text = element_text(size = 10),
        legend.background = element_rect(fill = NA)) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(shape = c(16, 16),
                                                  linetype = c(1, 1)),
                              keyheight = 0.03,
                              default.unit = "cm"))

figc

fig3 <- plot_grid(figa, figb, figc,
                  nrow = 3,
                  align = "v",
                  labels = "auto")
ggsave(filename = "fig_scripts/round2/fig3.png",
       plot = fig3,
       height = 7,
       width = 8,
       units = "in")



