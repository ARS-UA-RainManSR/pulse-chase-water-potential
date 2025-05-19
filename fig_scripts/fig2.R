# 2 or 3 panel plot with
# a) moisture release curves by depth (WP4 data + Gardner models)
# b) double axis timeseries SWP and VWC by depth, for whole pulse period
# boxes to highlight pulse chase duration?
# c or supplemental) comparison of predicted and measured SWP for house with data in 2023 pulse chase

library(tidyverse)
source("source/vwc2swp.R")
library(RColorBrewer)
library(cowplot)

#### Fig A: moisture release curves by depth ####

# Read in raw WP4 data
wp4 <- read_csv("data_clean/moisture_release.csv") |> 
  mutate(Depth = case_when(depth == 10 ~ "10 cm",
                      depth == 25 ~ "25 cm"))

# Create predicted data
mrc <- data.frame(vwc = seq(0.015, 0.092, by = 0.0005)) |> 
  mutate(swp_10 = vwc2swp(vwc, param = "depth10cm"),
         swp_25 = vwc2swp(vwc, param = "depth25cm")) |> 
  pivot_longer(starts_with("swp"),
               names_to = "depth",
               values_to = "swp") |> 
  mutate(Depth = case_when(depth == "swp_10" ~ "10 cm",
                           depth == "swp_25" ~ "25 cm"))

# colors
cols_pur <-brewer.pal(7, "PRGn")
display.brewer.pal(7, "PRGn")

fig_a <- wp4 |> 
  filter(vwc > 0.015) |> # limit for lookup table
  ggplot() +
  geom_point(aes(x = vwc, y = -mpa, color = Depth)) +
  geom_line(data = mrc,
            aes(x = vwc, y = -swp, color = Depth)) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)")),
                     limits = c(0, 4)) +
  scale_color_manual(values = cols_pur[2:1]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.65, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

fig_a

#### Fig B: SWP and VWC timeseries ####

# Load instrument/predicted timeseries for study period
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         depth != "75 cm",
         summer != "S3",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  filter(period == "morn",
         depth != "75 cm",
         summer != "S3",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Create shading boxes for each pulse

pulse <- data.frame(summer = c("S4", "S1", "S2", "S1", "S2"),
                    trt_label = c("P21", "P3.5", "P7", "P3.5", "P7"),
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
                           as.Date("2023-08-28", tz = "America/Phoenix"))) |>
  mutate(trt_label = factor(trt_label, levels = c("P3.5", "P7", "P21")))


# colors
cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")

# labels
labs <- c(lapply(c("soil"), function(i) bquote(Psi[.(i)])),
          bquote(Theta))


fig_b <- ggplot() +
  geom_rect(data = pulse,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1) +
  geom_line(data = vwc,
            aes(x = date, y = mean,
                color = "VWC",
                lty = depth)) +
  geom_line(data = swp,
            aes(x = date, y = (mean + 2.7)/20, # 15 and 2 also work well, but show negative vwc's
                color = "SWP",
                lty = depth)) +
  scale_y_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")),
                     sec.axis = sec_axis(~.*20-2.7,
                                         expression(paste(Psi[soil], " (MPa)")))) +
  scale_x_date(breaks = as.Date(c("2023-08-14", "2023-08-21", 
                                  "2023-08-28")),
               date_labels = "%b %d") +
  scale_color_manual(values = cols_br_gn[1:2],
                     label = labs) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  facet_grid(cols = vars(trt_label),
             rows = vars(depth),
             scales = "free_y",
             space = "free_y") +
  guides(linetype = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        # strip.placement.x = "outside",
        legend.title = element_blank(),
        legend.position = c(0.25, 0.93),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.35, "cm"),
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill = "transparent"),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = cols_br_gn[2]),
        axis.title.y.right = element_text(color = cols_br_gn[1]))


#### Combine and print out ####

fig2 <- plot_grid(fig_a, fig_b, ncol = 2,
          rel_widths = c(1, 3),
          align = "h", axis = "bt",
          labels = "auto")

ggsave("fig_scripts/round2/fig2.png",
       fig2,
       height = 4,
       width = 10,
       units = "in")
#### Version of fig_b with only VWC for AGU talk
fig_c <- ggplot() +
  geom_rect(data = pulse,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1) +
  geom_line(data = vwc,
            aes(x = date, y = mean,
                color = "VWC",
                lty = depth)) +
  scale_y_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")),
                     breaks = c(0, 0.05, 0.10)) +
  # geom_line(data = swp,
  #           aes(x = date, y = (mean + 2.7)/20, # 15 and 2 also work well, but show negative vwc's
  #               color = "SWP",
  #               lty = depth)) +
  # scale_y_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")),
  #                    sec.axis = sec_axis(~.*20-2.7,
  #                                        expression(paste(Psi[soil], " (MPa)")))) +
  scale_x_date(breaks = as.Date(c("2023-08-14", "2023-08-21", 
                                  "2023-08-28")),
               date_labels = "%b %d") +
  scale_color_manual(values = cols_br_gn[2],
                     label = labs) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  facet_grid(cols = vars(trt_label),
             rows = vars(depth),
             scales = "free_y",
             space = "free_y") +
  guides(linetype = "none",
         color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        # strip.placement.x = "outside",
        legend.title = element_blank(),
        legend.position = c(0.25, 0.93),
        legend.background = element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.35, "cm"),
        strip.background.x = element_blank(),
        strip.background.y = element_rect(fill = "transparent"),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = cols_br_gn[2]),
        axis.title.y.right = element_text(color = cols_br_gn[1]))

fig2b <- plot_grid(fig_a, fig_c, ncol = 2,
                  rel_widths = c(1, 3),
                  align = "h", axis = "bt",
                  labels = "auto")

ggsave("fig_scripts/fig2_AGU.png",
       fig2b,
       height = 4,
       width = 10,
       units = "in")
