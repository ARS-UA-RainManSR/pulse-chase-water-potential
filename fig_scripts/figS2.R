# Fig S1
# Plot supplemental Fig wth DICA traits
library(tidyverse)

# Load data
dica <- read_csv("data_clean/dica_traits.csv") |>
  mutate(trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))
table(dica$trt_label)

# Long version
dica_long <- dica |>
  select(-diam_1, -diam_2) |>
  pivot_longer(height:diam,
               names_to = "variable",
               values_to = "value") |>
  mutate(var2 = case_when(variable == "height" ~ "Height (cm)",
                          variable == "diam" ~ "Diameter (mm)",
                          variable == "culm_n" ~ "# of culms"))

# Summarize by trait
dica_sum <- dica_long |>
  group_by(var2, trt_label) |>
  summarize(var_m = mean(value),
            var_sd = sd(value))

# Make plot
figS2 <- ggplot() +
  geom_point(data = dica_long,
             aes(x = trt_label, 
                 y = value),
             alpha = 0.25,
             position = position_jitter(width = 0.25)) +
  geom_pointrange(data = dica_sum,
                  aes(x = trt_label,
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


ggsave(filename = "fig_scripts/round2/figS2.png",
       plot = figS2,
       height = 6,
       width = 3,
       units = "in")
