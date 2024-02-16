#### Test vwc2swp.R on real data
# On inputs from WP4 
# and on instrumented vWC-SWP pairs

library(tidyverse)
source("source/vwc2swp.R")

# Load original input data
# Read in data
mrc <- read_csv("data_clean/moisture_release.csv") |> 
  rename(wp = mpa)

range(mrc$vwc)

# Create column of predicted swp
preds <- mrc |> 
  filter(vwc > 0.015) |> 
  mutate(swp_site = vwc2swp(vwc, param = "site", stat = "median"),
         swp_depth = case_when(depth == 10 ~ vwc2swp(vwc, param = "depth10cm", stat = "median"),
                               depth == 25 ~ vwc2swp(vwc, param = "depth25cm", stat = "median")))


preds %>%
  ggplot(aes(x = vwc)) +
  geom_point(aes(y = wp,
                 col = "actual")) +
  geom_point(aes(y = swp_site,
                 col = "site")) +
  geom_point(aes(y = swp_depth,
                 col = "by depth")) +
  theme_bw(base_size = 14)


# Load in automated data from house 3
mrc_auto <- read_csv("data_clean/moisture_release_auto.csv") |> 
  mutate(depth = case_when(Depth == "1" ~ "10",
                           Depth == "2" ~ "25")) |> 
  mutate(house = as.character(House),
         swp_site = vwc2swp(WC_mean, param = "site", stat = "median"),
         swp_depth = case_when(depth == 10 ~ vwc2swp(WC_mean, param = "depth10cm", stat = "median"),
                               depth == 25 ~ vwc2swp(WC_mean, param = "depth25cm", stat = "median")))


mrc_auto |> 
  ggplot() +
  geom_errorbar(aes(x = WC_mean,
                    ymin = WP_min,
                    ymax = WP_max),
                alpha = 0.1) +
  geom_point(aes(x = WC_mean,
                 y = WP_mean, color = "in situ")) +
  geom_line(aes(x = WC_mean, y = swp_site, color = "site"),
            linewidth = 1, alpha = 0.75) +
  geom_line(aes(x = WC_mean, y = swp_depth, color = "by depth"),
            linewidth = 1, alpha = 0.75) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)"))) +
  scale_color_manual(values = c("coral", "black", "skyblue")) +
  facet_wrap(~depth) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

# Quick stats
m1 <- lm(swp_depth ~ WP_mean, data = mrc_auto)
summary(m1)

m2 <- lm(swp_site ~ WP_mean, data = mrc_auto)
summary(m2)
