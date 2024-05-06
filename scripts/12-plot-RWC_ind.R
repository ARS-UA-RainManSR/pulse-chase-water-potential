# Check further plots of RWC_ind and RWC/WP 
# scatterplot by phase

library(tidyverse)
library(cowplot)


# load data files
# indices from hyperspectral
hyp_ind <- read_csv("data_clean/hyp_indices.csv") |> 
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix"))


# irrigation
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4"))

# Load SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(hyp_ind$date_col),
         date <= max(hyp_ind$date_col),
         depth == "0-12 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         SWP_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)



# Join with long format of rwc and wp
# join with WP long cleaned - need pulse variables
hyp_wide <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |>
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  pivot_wider(names_from = variable,
              values_from = value) |>
  # Join hyperspectral
  left_join(hyp_ind) |>
  # Join SWP
  left_join(swp, by = join_by("date_col" == "date", "trt_s")) |>
  # dummy vector to connect the points
  # label with phase determined by SWP < -1 MPa
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2),
         phase = if_else(SWP_1 <= -1, "Phase 2", "Phase 1")) 


# Plot scatterplot

hyp_wide |>
  ggplot(aes(x = RWC_ind, y = RWC)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~phase)


hyp_wide |>
  ggplot(aes(x = RWC, y = WP)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~phase)

# histogram distribution of RWC_ind
hyp_wide |>
  ggplot(aes(x = RWC_ind)) +
  geom_histogram() +
  facet_wrap(~phase)

hyp_wide |>
  ggplot(aes(x = RWC)) +
  geom_histogram() +
  facet_wrap(~phase)
