# Clean data on DICA traits
# Quick exploration

library(tidyverse)

# Tidy to only Jessica plants
dica <- read_csv("data/2023-09-23_dica_structural_traits.csv") |>
  select(1:10) |>
  mutate(ID = paste0("H", House, "P", Plot),
         trt_s = paste0("S", `Summer P`),
         trt_w = paste0("W", `Winter P`)) |>
  rename(plant = `JESSICA ID`) |>
  select(-3:-4) |>
  filter(!is.na(plant),
         plant != ".") |>
  mutate(plant = toupper(plant),
         height = as.numeric(Height),
         diam_1 = as.numeric(`Diam 1`),
         diam_2 = as.numeric(`Diam 2`),
         culm_n = as.numeric(`# rep culms`)) |>
  select(-4:-7) |>
  group_by(ID, plant) |>
  mutate(diam = mean(diam_1, diam_2))

# Write out
write_csv(dica, "data_clean/dica_traits.csv")

# Explore data
table(dica$ID)
# H1P2 and H3P6 started out with 3 DICA apiece
# But the D3 was small and sampled sparingly or not at all

dica |>
  ggplot(aes(x = ID, y = height)) +
  geom_point(aes(color = trt_s)) +
  facet_wrap(~trt_s, 
             scale = "free_x",
             ncol = 1)

dica |>
  ggplot(aes(x = ID, y = diam)) +
  geom_point(aes(color = trt_s)) +
  facet_wrap(~trt_s, 
             scale = "free_x",
             ncol = 1)

dica |>
  ggplot(aes(x = ID, y = culm_n)) +
  geom_point(aes(color = trt_s)) +
  facet_wrap(~trt_s, 
             scale = "free_x",
             ncol = 1)

# Make long version
dica_long <- dica |>
  select(-diam_1, -diam_2) |>
  pivot_longer(height:diam,
               names_to = "variable",
               values_to = "value") |>
  mutate(var2 = case_when(variable == "height" ~ "Height (cm)",
                          variable == "diam" ~ "Diameter (mm)",
                          variable == "culm_n" ~ "# of culms"))

dica_sum <- dica_long |>
  group_by(var2, trt_s) |>
  summarize(var_m = mean(value),
            var_sd = sd(value))


ggplot() +
  geom_point(data = dica_long,
              aes(x = trt_s, 
                  y = value),
             alpha = 0.25,
              position = position_jitter(width = 0.25)) +
  geom_pointrange(data = dica_sum,
                  aes(x = trt_s,
                      y = var_m,
                      ymin = var_m - var_sd,
                      ymax = var_m + var_sd)) +
  scale_x_discrete("Treatment") +
  facet_wrap(~var2, 
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12))

