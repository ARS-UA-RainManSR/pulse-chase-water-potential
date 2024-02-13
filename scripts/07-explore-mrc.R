# Exploratory plotting of moisture release curves

library(readxl)
library(tidyverse)


# Read in moisture release data
mrc <- read_excel("data/moisture_release_curves.xlsx",
                  sheet = "Plots") |> 
  filter(grepl("H", sample_id)) |> # Only soils from plots
  rename(cup_dry_soil_g = "cup_dry soil_g") |> 
  mutate(gwc = (cup_soil_g - cup_dry_soil_g) / (cup_dry_soil_g - cup_g),
         vwc = gwc * 1.8) |>  # convert to volumetric using bulk density of 1.8
  tidyr::separate(sample_id, 
                  into = c("ID", "depth")) |> 
  mutate(house = str_extract(ID, "(\\d)"))
  
# Quick plot 
# GWC
mrc |> 
  ggplot() +
  geom_point(aes(x = mpa, y = gwc, 
                 color = interaction(house, depth),
                 shape = house)) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_y_continuous(expression(paste("Gravimetric water content (g ", g^-1, ")"))) +
  theme_bw()

mrc |> 
  ggplot() +
  geom_point(aes(x = vwc, y = mpa, 
                 color = house)) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_x_continuous(expression(paste("Volumetric water content (g ", g^-1, ")"))) +
  facet_wrap(~depth) +
  theme_bw()

mrc |> 
  ggplot() +
  geom_point(aes(x = vwc, y = mpa, 
                 color = depth)) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_x_continuous(expression(paste("Volumetric water content (g ", g^-1, ")"))) +
  facet_wrap(~house) +
  theme_bw()


# Read in vwc and mrc-auto data to compare
swc <- read_csv("data_clean/vwc_daily_daytime.csv") |> 
  filter(period == "morn")
range(swc$mean, na.rm = TRUE)

mrc |> 
  ggplot() +
  geom_point(aes(x = vwc, y = mpa, 
                 color = depth)) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                     limits = c(-10, 0)) +
  scale_x_continuous(expression(paste("Volumetric water content (g ", g^-1, ")")),
                     limits = range(swc$mean, na.rm = TRUE)) +
  facet_wrap(~house, scales = "free_y") +
  theme_bw()


mrc_auto <- read_csv("data_clean/moisture_release_auto.csv") |> 
  mutate(depth = case_when(Depth == "1" ~ "10",
                           Depth == "2" ~ "25")) |> 
  mutate(house = as.character(House))

ggplot() +
  geom_point(data = mrc, 
             aes(x = vwc, y = mpa, 
                 color = "WP4")) +
  geom_point(data = mrc_auto, 
             aes(x = WC_mean, y = WP_mean,
                 color = "auto")) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                     limits = c(-10, 0)) +
  scale_x_continuous(expression(paste("Volumetric water content (g ", g^-1, ")")),
                     limits = range(swc$mean, na.rm = TRUE)) +
  facet_grid(rows = vars(depth),
             cols = vars(house)) +
  theme_bw()


# Save out
write_csv(mrc, file = "data_clean/moisture_release.csv")
