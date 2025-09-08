# Comparison of RWC with multiple hyperspec water indices
# Use all treatments but subdivide in to Phase 1 and Phase 2

library(tidyverse)
library(ggh4x)
library(cowplot)
library(RColorBrewer)

# load data files
# indices from hyperspectral
hyp_ind <- read_csv("data_clean/hyp_indices.csv") |> 
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix"))

# load swp data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         depth %in% c("0-10 cm", "25 cm"),
         date >= min(hyp_ind$date_col),
         date <= max(hyp_ind$date_col)) |> 
  select(-period, -sd, depth) |> 
  pivot_wider(names_from = depth,
              values_from = mean) |>
  rename(trt_s = summer, SWP_1 = `0-10 cm`, SWP_2 = `25 cm`) 

# join with RWC long cleaned - need pulse variables
hyp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |> 
  filter(variable == "RWC") |> 
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  # Join with hyperspectral indices
  left_join(hyp_ind, by = join_by(date_col, period, house, plot, trt, ID,
                                  trt_s, trt_w)) |> 
  # Join with swp 
  left_join(swp, by = join_by(date_col == date,
                              trt_s == trt_s)) |>
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2),
         phase = case_when(SWP_1 > -1 ~ "Phase~1",
                           SWP_1 < -1 ~ "Phase~2")) |>
  select(-NDVI, -CI1, -CI2, -NDWI1, -NDWI2, -NDWI3, 
         -WI1, -WI2, -WI3, -PRI, -CNDI) |>
  pivot_longer(cols = WBI:RWC_ind,
               names_to = "index_name",
               values_to = "index_value") |>
  mutate(index_name2 = if_else(index_name == "RWC_ind", "RWC[ind]", index_name))


# Repeated lms on each index in Phase 2
lm_params <- hyp |>
  filter(phase == "Phase~2") |>
  nest_by(index_name2) |>
  mutate(model = list(lm(value ~ index_value, data = data))) |>
  summarize(broom::glance(model),
            slope = coef(summary(model))[2,1],
            intercept = coef(summary(model))[1,1]) |>
  mutate(phase = "Phase~2",
         sig = if_else(p.value < 0.05, TRUE, FALSE))

# Position text to report p value and R2
lm_text <- hyp |>
  filter(phase == "Phase~2") |>
  group_by(phase, index_name2) |>
  summarize(x = mean(index_value),
            y = 1) |>
  left_join(select(lm_params, 
                   index_name2, adj.r.squared, p.value, sig),
            by = join_by(index_name2)) |>
  mutate(lab1 = case_when(sig == FALSE ~ paste0("p==", round(p.value, 3)),
                          sig == TRUE ~ paste0("p<0.001")),
         lab2 = if_else(index_name2 != "WPI2", 
                        paste0("R^2==", round(adj.r.squared, 3)),
                        ""),
         x = case_when(index_name2 == "RWC[ind]" ~ x-0.035,
                       index_name2 == "WBI" ~ x - 0.005,
                       .default = x))
lm_text


# colors for figure
cols_gn <- brewer.pal(4, "Paired")
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_y = elem_list_rect(fill = cols_div[c(6,3)]))


figS4 <- ggplot() +
  geom_abline(data = lm_params,
              aes(slope = slope,
                  intercept = intercept,
                  lty = sig)) +
  geom_point(data = hyp,
             aes(y = value, # RWC
                 x = index_value,
                 color = period2)) +
  geom_text(data = lm_text,
            aes(x = x, y = y,
                label = lab1),
            parse = TRUE,
            vjust = 0,
            hjust = 0) +
  geom_text(data = lm_text,
            aes(x = x, y = y,
                label = lab2),
            parse = TRUE,
            vjust = 1,
            hjust = 0) +
  facet_grid2(rows = vars(phase),
             cols = vars(index_name2),
             scales = "free_x",
             switch = "x",
             labeller = label_parsed,
             strip = strip) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
                     limits = c(0.25, 1.1)) +
  scale_color_manual(values = cols_gn[4:3]) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(lty = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        # legend.position.inside = c(0.18, 0.6),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        strip.background.x = element_blank())

ggsave(filename = "fig_scripts/figS4.png",
       plot = figS4,
       height = 5,
       width = 8,
       units = "in")

# Version with only RWC and WBI
figs2a <-
  ggplot() +
  geom_abline(data = lm_params |>
                filter(index_name2 %in% c("RWC[ind]", "WBI")),
              aes(slope = slope,
                  intercept = intercept,
                  lty = sig)) +
  geom_point(data = hyp |>
               filter(index_name2 %in% c("RWC[ind]", "WBI")),
             aes(y = value, # RWC
                 x = index_value,
                 color = period2)) +
  geom_text(data = lm_text |>
              filter(index_name2 %in% c("RWC[ind]", "WBI")),
            aes(x = x, y = y,
                label = lab1),
            parse = TRUE,
            vjust = 0,
            hjust = 0) +
  geom_text(data = lm_text |>
              filter(index_name2 %in% c("RWC[ind]", "WBI")),
            aes(x = x, y = y,
                label = lab2),
            parse = TRUE,
            vjust = 1,
            hjust = 0) +
  facet_grid2(rows = vars(phase),
              cols = vars(index_name2),
              scales = "free_x",
              switch = "x",
              labeller = label_parsed,
              strip = strip) +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")")),
                     limits = c(0.25, 1.1)) +
  scale_color_manual(values = cols_gn[4:3]) +
  scale_linetype_manual(values = c("solid")) +
  guides(lty = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.4, 0.6),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        strip.background.x = element_blank())

# Try with WP 
# Make another joined df
hyp2 <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |> 
  filter(variable == "WP") |> 
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  # Join with hyperspectral indices
  left_join(hyp_ind, by = join_by(date_col, period, house, plot, trt, ID,
                                  trt_s, trt_w)) |> 
  # Join with swp 
  left_join(swp, by = join_by(date_col == date,
                              trt_s == trt_s)) |>
  # dummy vector to connect the points
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2),
         phase = case_when(SWP_1 > -1 ~ "Phase~1",
                           SWP_1 < -1 ~ "Phase~2")) |>
  select(-NDVI, -CI1, -CI2, -NDWI1, -NDWI2, -NDWI3, 
         -WI1, -WI2, -WI3, -PRI, -CNDI) |>
  pivot_longer(cols = WBI:RWC_ind,
               names_to = "index_name",
               values_to = "index_value") |>
  mutate(index_name2 = if_else(index_name == "RWC_ind", "RWC[ind]", index_name))

# Repeated lms on each index in Phase 2
lm_params2 <- hyp2 |>
  filter(phase == "Phase~2") |>
  nest_by(index_name2) |>
  mutate(model = list(lm(value ~ index_value, data = data))) |>
  summarize(broom::glance(model),
            slope = coef(summary(model))[2,1],
            intercept = coef(summary(model))[1,1]) |>
  mutate(phase = "Phase~2",
         sig = if_else(p.value < 0.05, TRUE, FALSE))

# Position text to report p value and R2
lm_text2 <- hyp2 |>
  filter(phase == "Phase~2") |>
  group_by(phase, index_name2) |>
  summarize(x = quantile(index_value, probs = 0.9),
            y = -1) |>
  left_join(select(lm_params2, 
                   index_name2, adj.r.squared, p.value, sig),
            by = join_by(index_name2)) |>
  mutate(lab1 = case_when(sig == FALSE ~ paste0("p==", round(p.value, 3)),
                          sig == TRUE ~ paste0("p<0.001")),
         lab2 = if_else(index_name2 != "WPI2", 
                        paste0("R^2==", round(adj.r.squared, 3)),
                        ""),
         x = case_when(index_name2 == "RWC[ind]" ~ x-0.035,
                       index_name2 == "WBI" ~ x - 0.005,
                       .default = x))
lm_text2

figs2b <- ggplot() +
  geom_abline(data = lm_params2 |>
                filter(index_name2 %in% c("WPI1", "WPI2")),
              aes(slope = slope,
                  intercept = intercept,
                  lty = sig)) +
  geom_point(data = hyp2 |>
               filter(index_name2 %in% c("WPI1", "WPI2")),
             aes(y = value, # WP
                 x = index_value,
                 color = period2)) +
  geom_text(data = lm_text2 |>
              filter(index_name2 %in% c("WPI1", "WPI2")),
            aes(x = x, y = y,
                label = lab1),
            parse = TRUE,
            vjust = 0,
            hjust = 0) +
  geom_text(data = lm_text2 |>
              filter(index_name2 %in% c("WPI1", "WPI2")),
            aes(x = x, y = y,
                label = lab2),
            parse = TRUE,
            vjust = 1,
            hjust = 0) +
  facet_grid2(rows = vars(phase),
              cols = vars(index_name2),
              scales = "free_x",
              switch = "x",
              labeller = label_parsed,
              strip = strip) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = cols_gn[4:3]) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  guides(lty = "none") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.4, 0.6),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        strip.background.x = element_blank())

figS4new <- plot_grid(figs2a, figs2b,
          ncol = 1,
          align = "v")

ggsave(filename = "fig_scripts/figS4_new.png",
       plot = figS4new,
       height = 8,
       width = 5.5,
       units = "in")
