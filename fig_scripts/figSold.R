# Spectral indices as timeseries by treatment
# As either 3 or 5 rows

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
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc, -variable, -value) |> 
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
  summarize(NDVI_m = mean(NDVI, na.rm = TRUE),
            NDVI_sd = sd(NDVI, na.rm = TRUE),
            NDVI_n = sum(length(!is.na(NDVI))),
            NDWI1_m = mean(NDWI1, na.rm = TRUE),
            NDWI1_sd = sd(NDWI1, na.rm = TRUE),
            NDWI1_n = sum(length(!is.na(NDWI1))),
            NDWI2_m = mean(NDWI2, na.rm = TRUE),
            NDWI2_sd = sd(NDWI2, na.rm = TRUE),
            NDWI2_n = sum(length(!is.na(NDWI2))),
            NDWI3_m = mean(NDWI3, na.rm = TRUE),
            NDWI3_sd = sd(NDWI3, na.rm = TRUE),
            NDWI3_n = sum(length(!is.na(NDWI3))),
            PRI_m = mean(PRI, na.rm = TRUE),
            PRI_sd = sd(PRI, na.rm = TRUE),
            PRI_n = sum(length(!is.na(PRI))))

##### NDVI #####
figa <- 
  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = NDVI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = NDVI_m - NDVI_sd,
                    ymax = NDVI_m + NDVI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = NDVI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = NDVI_m,
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
        legend.position = c(0.65, 0.25),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

##### NDWI2 #####
figb <-
  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = NDWI2,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = NDWI2_m - NDWI2_sd,
                    ymax = NDWI2_m + NDWI2_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = NDWI2_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = NDWI2_m,
                color = period2,
                group = interaction(pulse_num2, period2))) +
  scale_y_continuous("NDWI") +
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
        legend.position = c(0.9, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))

##### PRI #####
figc <-
  ggplot() +
  geom_vline(data = irig,
             aes(xintercept = date),
             lty = "dotted")  +
  geom_point(data = hyp,
             aes(x = date_col,
                 y = PRI,
                 color = period2),
             alpha = 0.25,
             position = "jitter") +
  geom_errorbar(data = hyp_sum,
                aes(x = date_col,
                    ymin = PRI_m - PRI_sd,
                    ymax = PRI_m + PRI_sd,
                    color = period2),
                alpha = 0.5, width = 0.75) +
  geom_point(data = hyp_sum,
             aes(x = date_col,
                 y = PRI_m,
                 color = period2),
             size = 2.5) +
  geom_line(data = hyp_sum,
            aes(x = date_col,
                y = PRI_m,
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
        legend.position = c(0.9, 0.15),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = NA))


##### Arrange and print #####
figS2 <- plot_grid(figa, figb, figc,
          ncol = 1,
          align = "v",
          labels = "auto")

ggsave(filename = "fig_scripts/figSold.png",
       plot = figS2,
       height = 6,
       width = 6,
       units = "in")

