# Create figures of new water indices
library(tidyverse)
library(cowplot)

# library(ggpubr)

# load data files
# indices from hyperspectral
hyp_ind <- read_csv("data_clean/hyp_indices.csv") |> 
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix"))


# join with WP long cleaned - need pulse variables
hyp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |> 
  filter(variable == "WP") |> 
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  left_join(hyp_ind) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2))



# irrigation
irig <- read_csv("data_clean/irig_long.csv") |> 
  filter(irig > 0,
         date %in% c(as.Date("2023-08-14"),
                     as.Date("2023-08-21")),
         trt_s %in% c("S1", "S2", "S4"))

# Summarize indices by date and treatment
hyp_sum <- hyp |> 
  group_by(date_col, pulse_num2, trt_s, period2) |> 
  summarize(WBI_m = mean(WBI, na.rm = TRUE),
            WBI_sd = sd(WBI, na.rm = TRUE),
            WBI_n = sum(length(!is.na(WBI))),
            WPI1_m = mean(WPI1, na.rm = TRUE),
            WPI1_sd = sd(WPI1, na.rm = TRUE),
            WPI1_n = sum(length(!is.na(WPI1))),
            WPI2_m = mean(WPI2, na.rm = TRUE),
            WPI2_sd = sd(WPI2, na.rm = TRUE),
            WPI2_n = sum(length(!is.na(WPI2))),
            RWC_ind_m = mean(RWC_ind, na.rm = TRUE),
            RWC_ind_sd = sd(RWC_ind, na.rm = TRUE),
            RWC_ind_n = sum(length(!is.na(RWC_ind))),
            CNDI_m = mean(CNDI, na.rm = TRUE),
            CNDI_sd = sd(CNDI, na.rm = TRUE),
            CNDI_n = sum(length(!is.na(CNDI))))

# WBI
figa <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = WBI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = WBI_m - WBI_sd,
                    ymax = WBI_m + WBI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = WBI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = WBI_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_grid(cols = vars(trt_s),
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  # guides(color = "none") +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.75),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# WPI1
figb <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = WPI1,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = WPI1_m - WPI1_sd,
                    ymax = WPI1_m + WPI1_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = WPI1_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = WPI1_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_grid(cols = vars(trt_s),
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# WPI2
figc <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = WPI2,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = WPI2_m - WPI2_sd,
                    ymax = WPI2_m + WPI2_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = WPI2_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = WPI2_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_grid(cols = vars(trt_s),
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.85),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# RWC_ind
figd <- ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = RWC_ind,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = RWC_ind_m - RWC_ind_sd,
                    ymax = RWC_ind_m + RWC_ind_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = RWC_ind_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = RWC_ind_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_grid(cols = vars(trt_s),
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        # legend.position = c(0.85, 0.85),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

# CNDI
ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = CNDI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = CNDI_m - CNDI_sd,
                    ymax = CNDI_m + CNDI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = CNDI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = CNDI_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_x_date(date_labels = "%b %d",
               breaks = seq(as.Date("2023-08-14"),
                            as.Date("2023-09-04"), 
                            by = 7)) +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_grid(cols = vars(trt_s),
             scales = "free_x",
             space = "free_x") +
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.85),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

figS4 <- plot_grid(figa, figb, figd, 
                   ncol = 1,
                   align = "v",
                   labels = "auto")

ggsave(filename = "fig_scripts/figS4.png",
       plot = figS4,
       height = 6,
       width = 6,
       units = "in")

##### Scatterplots ####

# Join with long format of rwc and wp
# join with WP long cleaned - need pulse variables
hyp_long <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |>
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  left_join(hyp_ind) |> 
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2))


fig1 <- hyp_long |>
    ggplot(aes(x = WBI, y = value)) +
    geom_point(aes(color = period2)) +
    facet_wrap(~variable, scales = "free_y",
               strip.position = "left") +
    scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
    guides(color = "none") +
    theme_bw(base_size = 14) +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.title.y = element_blank())

fig2 <- hyp_long |>
  ggplot(aes(x = WPI1, y = value)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

fig3 <- hyp_long |>
  ggplot(aes(x = WPI2, y = value)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

fig4 <- hyp_long |>
  ggplot(aes(x = RWC_ind, y = value)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

figS5 <- plot_grid(fig1, fig2, fig3, fig4,
                   ncol = 2,
                   align = "v",
                   labels = "auto")

# Calculate means and add to existing bivariate plots
hyp_long_sum <- hyp_long |>
  group_by(trt_s, pulse_num2, period2, date_col, variable) |>
  summarize(var_m = mean(value, na.rm = TRUE),
            var_sd = sd(value, na.rm = TRUE)) |>
  left_join(hyp_sum, by = join_by("date_col", "period2", "trt_s", 
                                  "pulse_num2"))


# fig1 <- 
hyp_long |>
  ggplot() +
  geom_point(aes(x = WBI, y = value, color = period2),
             alpha =  0.25) +
  geom_errorbar(data = hyp_long_sum,
                aes(x = WBI_m,
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    col = period2)) +
  geom_errorbarh(data = hyp_long_sum,
                 aes(xmin = WBI_m - WBI_sd, xmax = WBI_m + WBI_sd,
                     y = var_m,
                     col = period2)) +
  geom_point(data = hyp_long_sum,
             aes(x = WBI_m, y = var_m, col = period2),
             size = 2) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

hyp_long |>
  ggplot() +
  geom_point(aes(x = WPI1, y = value, color = period2),
             alpha =  0.25) +
  geom_errorbar(data = hyp_long_sum,
                aes(x = WPI1_m,
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    col = period2)) +
  geom_errorbarh(data = hyp_long_sum,
                 aes(xmin = WPI1_m - WPI1_sd, xmax = WPI1_m + WPI1_sd,
                     y = var_m,
                     col = period2)) +
  geom_point(data = hyp_long_sum,
             aes(x = WPI1_m, y = var_m, col = period2),
             size = 2) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

hyp_long |>
  ggplot() +
  geom_point(aes(x = WPI2, y = value, color = period2),
             alpha =  0.25) +
  geom_errorbar(data = hyp_long_sum,
                aes(x = WPI2_m,
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    col = period2)) +
  geom_errorbarh(data = hyp_long_sum,
                 aes(xmin = WPI2_m - WPI2_sd, xmax = WPI2_m + WPI2_sd,
                     y = var_m,
                     col = period2)) +
  geom_point(data = hyp_long_sum,
             aes(x = WPI2_m, y = var_m, col = period2),
             size = 2) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

hyp_long |>
  ggplot() +
  geom_point(aes(x = RWC_ind, y = value, color = period2),
             alpha =  0.25) +
  geom_errorbar(data = hyp_long_sum,
                aes(x = RWC_ind_m,
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    col = period2)) +
  geom_errorbarh(data = hyp_long_sum,
                 aes(xmin = RWC_ind_m - RWC_ind_sd, xmax = RWC_ind_m + RWC_ind_sd,
                     y = var_m,
                     col = period2)) +
  geom_point(data = hyp_long_sum,
             aes(x = RWC_ind_m, y = var_m, col = period2),
             size = 2) +
  facet_wrap(~variable, scales = "free_y",
             strip.position = "left") +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  guides(color = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank())

#### Matched time series plots ####

# S4- only 
# compare LWP, RWC, WBI, and RWC_ind

fig_1 <- hyp_long |>
  filter(trt_s == "S4") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S4"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = hyp_long_sum |>
                  filter(trt_s == "S4"),
             aes(x = date_col, 
                 ymin = var_m - var_sd, ymax = var_m + var_sd,
                 color= period2),
             width = 0) +
  geom_point(data = hyp_long_sum |>
               filter(trt_s == "S4"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = hyp_long_sum |>
               filter(trt_s == "S4"),
             aes(x = date_col, y = var_m,
                 color= period2)) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")

# Make long version of only WBI and RWC_ind
sub_long <- hyp |>
  select(1:17, ID, WBI, RWC_ind, pulse_num2) |>
  relocate(pulse_num2, .before = pulse_num) |>
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |>
  select(-value, -variable) |>
  pivot_longer(WBI:RWC_ind, 
               names_to = "variable",
               values_to = "value")

sub_long_sum <- sub_long |>
  group_by(date_col, pulse_num2, trt_s, period2, variable) |>
  summarize(var_m = mean(value, na.rm = TRUE),
            var_sd = sd(value, na.rm = TRUE))

fig_2 <- sub_long |>
  filter(trt_s == "S4") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S4"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = sub_long_sum |>
                  filter(trt_s == "S4"),
                aes(x = date_col, 
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    color= period2),
                width = 0) +
  geom_point(data = sub_long_sum |>
               filter(trt_s == "S4"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = sub_long_sum |>
              filter(trt_s == "S4"),
            aes(x = date_col, y = var_m,
                color= period2)) +
  scale_y_reverse() +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")

plot_grid(fig_1, fig_2,
          ncol = 2,
          align = "v",
          labels = "auto")

# S2- only 
# compare LWP, RWC, WBI, and RWC_ind

fig_3 <- hyp_long |>
  filter(trt_s == "S2") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S2"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = hyp_long_sum |>
                  filter(trt_s == "S2"),
                aes(x = date_col, 
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    color= period2),
                width = 0) +
  geom_point(data = hyp_long_sum |>
               filter(trt_s == "S2"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = hyp_long_sum |>
              filter(trt_s == "S2"),
            aes(x = date_col, y = var_m,
                color= period2,
                group = interaction(pulse_num2, period2))) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")


fig_4 <- sub_long |>
  filter(trt_s == "S2") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S2"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = sub_long_sum |>
                  filter(trt_s == "S2"),
                aes(x = date_col, 
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    color= period2),
                width = 0) +
  geom_point(data = sub_long_sum |>
               filter(trt_s == "S2"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = sub_long_sum |>
              filter(trt_s == "S2"),
            aes(x = date_col, y = var_m,
                color= period2,
                group = interaction(pulse_num2, period2))) +
  scale_y_reverse() +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")

plot_grid(fig_3, fig_4,
          ncol = 2,
          align = "v",
          labels = "auto")

# S1- only 
# compare LWP, RWC, WBI, and RWC_ind

fig_5 <- hyp_long |>
  filter(trt_s == "S1") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S1"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = hyp_long_sum |>
                  filter(trt_s == "S1"),
                aes(x = date_col, 
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    color= period2),
                width = 0) +
  geom_point(data = hyp_long_sum |>
               filter(trt_s == "S1"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = hyp_long_sum |>
              filter(trt_s == "S1"),
            aes(x = date_col, y = var_m,
                color= period2,
                group = interaction(pulse_num2, period2))) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")
fig_5

fig_6 <- sub_long |>
  filter(trt_s == "S1") |>
  ggplot() +
  geom_vline(data = irig |> filter(trt_s == "S1"),
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(aes(x = date_col, y = value,
                 color = period2),
             alpha = 0.25) +
  geom_errorbar(data = sub_long_sum |>
                  filter(trt_s == "S1"),
                aes(x = date_col, 
                    ymin = var_m - var_sd, ymax = var_m + var_sd,
                    color= period2),
                width = 0) +
  geom_point(data = sub_long_sum |>
               filter(trt_s == "S1"),
             aes(x = date_col, y = var_m,
                 color= period2),
             size = 2) +
  geom_line(data = sub_long_sum |>
              filter(trt_s == "S1"),
            aes(x = date_col, y = var_m,
                color= period2,
                group = interaction(pulse_num2, period2))) +
  scale_y_reverse() +
  scale_x_date() +
  scale_color_manual(values = RColorBrewer::brewer.pal(4, "Paired")[4:3]) +
  facet_wrap(~variable,
             ncol = 1,
             scales = "free_y",
             strip.position = "left") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank()) +
  guides(color = "none")

plot_grid(fig_5, fig_6,
          ncol = 2,
          align = "v",
          labels = "auto")

