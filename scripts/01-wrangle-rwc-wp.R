library(googlesheets4)
library(tidyverse)


# Read in water potential
WP <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=0",
                 sheet = "Water potential")
# Read in RWC
rwc <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=0",
                     sheet = "RWC") %>%
  mutate(mass_fresh = vial_cap_fresh_g - vial_cap_g,
         mass_dry = vial_dry_g - vial_g,
         RWC = (mass_fresh - mass_dry) / (saturated_g - mass_dry)) %>%
  left_join(WP, 
            join_by(date_col, period, house, plot, trt, plant))

# Quick plot to check relationship
ggplot(rwc, aes(x = RWC, y = wp_mpa)) +
  geom_point(aes(col = period))
