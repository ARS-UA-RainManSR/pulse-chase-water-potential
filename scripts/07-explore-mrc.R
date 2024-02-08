# Exploratory plotting of moisture release curves

library(readxl)
library(tidyverse)

mrc <- read_excel("data/moisture_release_curves.xlsx",
                  sheet = "Plots") |> 
  filter(grepl("H", sample_id)) |> # Only soils from plots
  rename(cup_dry_soil_g = "cup_dry soil_g") |> 
  mutate(gwr = (cup_soil_g - cup_dry_soil_g) / (cup_dry_soil_g - cup_g)) |> 
  tidyr::separate(sample_id, 
                  into = c("ID", "depth")) |> 
  mutate(house = str_extract(ID, "(\\d)"))
  
mrc |> 
  ggplot() +
  geom_point(aes(x = mpa, y = gwc, color = house)) +
  facet_wrap(~depth)

mrc |> 
  ggplot() +
  geom_point(aes(x = mpa, y = gwc, color = depth)) +
  facet_wrap(~house)

mrc |> 
  ggplot() +
  geom_point(aes(x = mpa, y = gwc, 
                 color = interaction(house, depth),
                 shape = house)) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_y_continuous(expression(paste("Gravimetric water content (g ", g^-1, ")"))) +
  theme_bw()

