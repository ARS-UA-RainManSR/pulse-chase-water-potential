library(tidyverse)

# Read in long data with indices
all <- read_csv("data_clean/spectra_ind_wp_rwc.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")),
         trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# MD vs. PD
temp <- all |> 
  select(date_col,ID, period2, trt, trt_label, WP) |> 
  pivot_wider(names_from = period2, values_from = WP)

temp |> 
  ggplot(aes(x = PD, y = MD,
             color = trt_label)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~trt_label)

m <- lm(MD ~ PD * trt_label, data = temp)
summary(m)

#### Characteristics of PD - MD ####
temp |> mutate(diff = PD - MD) |> pull(diff) |> summary()

temp |> mutate(diff = PD - MD) |> 
  ggplot(aes(x = date_col, y = diff, color = trt_label)) +
  geom_point() +
  facet_wrap(~trt_label)
