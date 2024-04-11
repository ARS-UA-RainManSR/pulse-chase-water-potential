# Supplemental plot of reflectances to indicate regions of indices
# Run 05-calculate-indices.R for a single date

library(tidyverse)

regions <- data.frame(lab = c("Vis", "NIR", "SWIR"),
                      st = c(400, 700, 1100),
                      en = c(700, 1100, 2400)) |>
  mutate(midx = (st + en)/2)

dataset |>
  filter(summer == "S4") |>
  mutate(period = case_when(time == "midday" ~ "MD",
                            time == "predawn" ~ "PD") |>
           factor(levels = c("PD", "MD"))) |>
  ggplot() +
  geom_rect(data = regions, 
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = 0.0025,
                fill = lab),
            alpha = 0.5) +
  geom_vline(aes(xintercept = 970, lty = "WBI")) +
  geom_vline(aes(xintercept = 900, lty = "WBI")) +
  geom_vline(aes(xintercept = 665, lty = "WPI")) +
  geom_vline(aes(xintercept = 715, lty = "WPI")) +
  geom_vline(aes(xintercept = 1457, lty = "WPI")) +
  geom_vline(aes(xintercept = 1430, lty = "RWC")) +
  geom_vline(aes(xintercept = 1850, lty = "RWC")) +
  geom_label(data = regions,
             aes(x = midx, y = 0.0025, label = lab),
             vjust = 1) +
  geom_point(aes(x = wavelength, y = reflectance,
                 group = full, color = period),
             size = 0.25) +
  facet_wrap(~summer) +
  scale_y_continuous("Reflectance") +
  scale_x_continuous("Wavelength (nm)", 
                     limits = c(400, 2400), 
                     breaks = seq(400, 2400, 400)) +
  scale_linetype_manual(name = "Indices", labels = c("RWC", "WBI", "WPI"), 
                        values = c("RWC" = 3, "WBI" = 1, "WPI" = 2)) +
  scale_fill_manual(values = c("Vis" = "darkseagreen", 
                               "NIR" = "darksalmon", 
                               "SWIR" = "darkred")) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  guides(linetype = guide_legend(override.aes = list(shape = NA)),
         color = guide_legend(override.aes = list(size = 2)),
         fill = "none")
