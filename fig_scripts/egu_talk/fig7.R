# Final figure?
# Show amount of time above and below the SWP threshold
# Across treatments

library(tidyverse)
library(RColorBrewer)
library(ggh4x)

# Load data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |>
  filter(period == "morn",
         summer %in% c("S1", "S2", "S4"),
         depth %in% c("0-12 cm", "25 cm")) |>
  mutate(phase = case_when(depth == "0-12 cm" & mean > -1 ~ "Phase 1",
                           depth == "0-12 cm" & mean <= -1 ~ "Phase 2",
                           depth == "25 cm" & mean > -1 ~ "Phase 1",
                           depth == "25 cm" & mean <= -1 ~ "Phase 2")) |>
  drop_na()  |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Calculate days spent in each phase by treatment
num_text <- swp |>
  group_by(trt_label, depth, phase) |>
  count() |>
  mutate(lab = paste0(n, " days"),
         y = case_when(depth == "0-12 cm" & phase == "Phase 1" ~ -4.5,
                       depth == "0-12 cm" & phase == "Phase 2" ~ -5,
                       depth == "25 cm" & phase == "Phase 1" ~ -2,
                       depth == "25 cm" & phase == "Phase 2" ~ -2.5))

# Establish SWP thresholds by depth
thresh <- data.frame(depth = c("0-12 cm", "25 cm"),
                     swp_thresh = c(-1, -1)) # should it be the same?

# Make plot
cols_div <- brewer.pal(7, "Spectral")
display.brewer.pal(7, "Spectral")

swp_temp <- swp |>
  mutate(ymin = case_when(depth == "0-12 cm" ~ -1,
                          depth == "25 cm" ~ -1)) # should it be the same?

fig7 <- swp |>
  ggplot() +
  stat_difference(data = swp_temp,
                  aes(x = date,
                      ymax = mean,
                      ymin = ymin)) +
  geom_line(aes(x = date,
                y = mean)) +
  geom_text(data = num_text |> filter(phase == "Phase 1"),
            aes(x = as.Date("2023-09-04"), y = y,
                label = lab, color = "Phase 1"),
            hjust = 0) +
  geom_text(data = num_text |> filter(phase == "Phase 2"),
            aes(x = as.Date("2023-09-04"), y = y,
                label = lab, color = "Phase 2"),
            hjust = 0) +
  geom_hline(data = thresh,
             aes(yintercept = swp_thresh)) +
  facet_grid(cols = vars(trt_label),
             rows = vars(depth),
             scales = "free_y",
             space = "free_y") +
  scale_x_date(breaks = as.Date(c("2023-07-03",
                                  "2023-07-24",
                                  "2023-08-14",
                                  "2023-09-04")),
               date_labels = "%b %d") +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_color_manual(values = cols_div[c(6,3)]) +
  scale_fill_manual(values = cols_div[c(6,3)],
                    labels = c("Phase 1", "Phase 2")) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        strip.background = element_blank(),
        legend.position = c(5, 0.55))


ggsave(filename = "fig_scripts/egu_talk/fig7.png",
       plot = fig7,
       height = 4,
       width = 8,
       units = "in")


# Calculate duration? consecutive days?

# Days either layer is above threshold
swp |>
  mutate(above = case_when(depth == "0-12 cm" & mean > -1 |
                             depth == "25 cm" & mean > -0.6 ~ TRUE,
                           .default = FALSE)) |>
  group_by(trt_label, date) |>
  summarize(above_temp = max(above)) |>
  group_by(trt_label) |>
  summarize(phase1 = sum(above_temp))

# Days top is dry but 25 cm is above threshold
temp <- swp |>
  select(-sd, -phase) |>
  pivot_wider(names_from = depth,
              values_from = mean) |>
  rename(SWP_1 = '0-12 cm', SWP_2 = '25 cm') |>
  mutate(bottom_only = if_else(SWP_1 <= -1 & SWP_2 > -0.6, TRUE, FALSE)) |>
  group_by(trt_label) |>
  summarize(bottom_total = sum(bottom_only))


         