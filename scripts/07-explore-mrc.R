# Exploratory plotting of moisture release curves

library(readxl)
library(tidyverse)

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
  geom_point(aes(x = mpa, y = vwc, 
                 color = interaction(house, depth),
                 shape = house)) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_y_continuous(expression(paste("Volumetric water content (g ", g^-1, ")"))) +
  theme_bw()


# Save out
write_csv(mrc, file = "data_clean/moisture_release.csv")
