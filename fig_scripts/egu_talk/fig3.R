# 2 or 5 rows (variables) by 3 columns (treatments)
# timeseries of raw data + daily means +/- SD 
# might add SWP to LWP and VWC to RWC

library(tidyverse)
library(RColorBrewer)
library(cowplot)

# Read in long data (super clean!)
both <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")))

# Separate wp and rwc
wp <- both |> 
  filter(variable == "WP")|> 
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

rwc <- both |> 
  filter(variable == "RWC")

# Summarize both
wp_sum <- wp |> 
  group_by(date_col, pulse_num, trt_s, period2) |> 
  summarize(WP_m = mean(value, na.rm = TRUE),
            WP_sd = sd(value, na.rm = TRUE),
            WP_n = sum(length(!is.na(value)))) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2)) |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

rwc_sum <- rwc |> 
  group_by(date_col, pulse_num, trt_s, period2) |> 
  summarize(RWC_m = mean(value, na.rm = TRUE),
            RWC_sd = sd(value, na.rm = TRUE),
            RWC_n = sum(length(!is.na(value)))) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2)) |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

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
         date >= min(both$date_col),
         date <= max(both$date_col),
         depth != "75 cm",
         summer != "S3") |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21"))) 

# Volumetric water content
vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(both$date_col),
         date <= max(both$date_col),
         depth != "75 cm",
         summer != "S3") |> 
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

####  A) LWP + SWP ####

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])),
          lapply(c("0-12 cm", "25 cm"), function(i) bquote(Psi[.(i)])))

cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")

figa <-
  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = wp,
             aes(x = date_col,
                 y = value,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = wp_sum,
                aes(x = date_col,
                    ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = wp_sum,
             aes(x = date_col,
                 y = WP_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = wp_sum,
            aes(x = date_col,
                y = WP_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  geom_line(data = swp,
            aes(x = date,
                y = mean,
                color = depth,
                lty = depth)) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  facet_wrap(~trt_label,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-03"), 
                            by = 7)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(4, "Paired"))[1:2], 
                                cols_br_gn[1], cols_br_gn[1]),
                     breaks = c("PD", "MD", "0-12 cm", "25 cm"),
                     labels = labs) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(linetype = "none",
         # color = guide_legend(keyheight = 0.4),
         color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA),
                                                  linetype = c(1, 1, 1, 2))))
  
  ggsave(filename = "fig_scripts/egu_talk/fig3.png",
       plot = figa,
       height = 4,
       width = 8,
       units = "in")


#### B) RWC + VWC ####
labs2 <- c(lapply(c("PD", "MD"), function(i) bquote(RWC[.(i)])),
           lapply(c("0-12 cm", "25 cm"), function(i) bquote(Theta[.(i)])))


cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")


figb <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = rwc,
             aes(x = date_col,
                 y = value,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = rwc_sum,
                aes(x = date_col,
                    ymin = RWC_m - RWC_sd,
                    ymax = RWC_m + RWC_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = rwc_sum,
             aes(x = date_col,
                 y = RWC_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = rwc_sum,
            aes(x = date_col,
                y = RWC_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  geom_line(data = vwc,
            aes(x = date,
                y = mean*5,
                color = depth,
                lty = depth)) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
                     limits = c(0, 1),
                     sec.axis = sec_axis(~./5,
                                         name = expression(paste(Theta, " (", cm^3, cm^-3, ")")))) +  
  facet_wrap(~trt_s,
             nrow = 1) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-03"), 
                            by = 7)) +
  scale_color_manual(values = c(rev(RColorBrewer::brewer.pal(4, "Paired"))[1:2], cols_br_gn[2], cols_br_gn[2]), 
                     breaks = c("PD", "MD", "0-12 cm", "25 cm"),
                     labels = labs2) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.25, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA)) +
  guides(linetype = "none",
         color = guide_legend(override.aes = list(shape = c(16, 16, NA, NA),
                                                  linetype = c(1, 1, 1, 2))))

fig3 <- plot_grid(figa, figb, nrow = 2,
                  align = "v",
                  labels = "auto")

ggsave(filename = "fig_scripts/fig3.png",
       plot = fig3,
       height = 5,
       width = 8,
       units = "in")



