# gs, ET, and GPP vs PD or SWP
# faceted by treatment

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggh4x)


# Load SWP
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         depth == "0-10 cm",
         summer != "S3",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Load VPD
vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "inside",
         period == "morn",
         !is.na(mean)) |>  
  dplyr::select(-location) |> 
  rename(D_morn_mean = mean, D_morn_sd = sd)


# Load S1, S2, and S4 LWP data
wp_pd <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP",
         period == "predawn")

wp_delta <- read_csv("data_clean/wp_rwc_long.csv") |> 
  select(-time_wp, -time_rwc, -cID, -notes_wp, -notes_rwc, -plant) |> 
  filter(variable == "WP") |> 
  pivot_wider(names_from = period, values_from = value) |> 
  mutate(delta_psi = predawn - midday)

# Psi_soil threshold from fig4.R script
Psi_soil <- -0.917

# Load gs, rerun model, calculate resids
gst <- read_csv(file = "data_clean/gs_leaftemp.csv",
                locale = locale(tz = "America/Phoenix")) |>
  rename(ID = hp) |>
  mutate(date_col = as.Date(date)) |>
  group_by(date_col, trt_s, ID) |>
  summarize(gs = mean(conductance)) |> 
  left_join(swp, by = join_by(trt_s == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))|> 
  left_join(wp_pd, join_by(date_col, trt_s, ID)) |> 
  rename(predawn = value) |> 
  left_join(wp_delta |> 
              select(date_col, trt_s, ID, delta_psi), 
            join_by(date_col, trt_s, ID))

# Check high leverage pd values
outlier <- gst |> 
  relocate(predawn, .after = gs) |> 
  filter(phase == "Phase 1") |> 
  ungroup() |> 
  arrange(predawn) |> 
  slice_head(n = 1)

# High leverage point occurs on 8/26 in H1P2, S2 treatment
# Day of minimal difference between predawn and midday (-1.84 vs. -1.91)
# Even as mean SWP is -0.91. Already disconnected from soil?

gst_remove_outlier <- gst |> 
  anti_join(outlier)

gs.pd.phase <- lme4::lmer(gs ~ phase + predawn*phase + (1|ID), data = gst_remove_outlier)

gst_remove_outlier <- gst_remove_outlier |> 
  ungroup() |> 
  mutate(resids_gs = residuals(gs.pd.phase))

gst_sum <- gst_remove_outlier |>
  group_by(trt_s, date_col) |>
  summarize(gs_m = mean(gs),
            gs_sd = sd(gs),
            pd_m = mean(predawn),
            pd_sd = sd(predawn)) |>
  left_join(swp, by = join_by(trt_s == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))


# Load GPP, rerun both models, calcluate resids
gpp <- read_csv("data/plotgas2023.csv") |>
  mutate(date_col = lubridate::mdy(Date,
                                   tz = "America/Phoenix") |> 
           as.Date(),
         ER2 = ifelse(ER < 0, 0, ER), # Restrict ER to positive values
         GPP = -NEE + ER2,
         house = str_extract(Plot, "H[0-9]")) |> 
  filter(date_col >= min(swp$date),
         date_col <= max(swp$date)) |> 
  left_join(swp, by = join_by(PT == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd,
         ID = Plot) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))

et.swp.phase <- lme4::lmer(ET ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)
gpp.swp.phase <- lme4::lmer(GPP ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)

gpp <- gpp |> 
  ungroup() |> 
  mutate(resids_et = residuals(et.swp.phase),
         resids_gpp = residuals(gpp.swp.phase))

gpp_sum <- gpp |>
  group_by(PT, date_col) |>
  summarize(gpp_m = mean(GPP),
            gpp_sd = sd(GPP),
            et_m = mean(ET),
            et_sd = sd(ET)) |> 
  left_join(swp, by = join_by(PT == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))


#### New figure 7 ####
# Each flux on its own
# Faceted by phase, with color blocks
# Different symbols for leaf vs. plot level
# Accompanying residual plot with VPD, simple linear regression
my_labeller <- as_labeller(c(gs = "g[s]~(mmol~H[2]*O~m^-2~s^-1)",
                             et = "ET~(mmol~H[2]*O~m^-2~s^-1)",
                             gpp = "GPP~(mu*mol~CO[2]~m^-2~s^-1)",
                             `Phase 1` = "Phase~1",
                             `Phase 2` = "Phase~2"),
                           default = label_parsed)
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(6,3)])))


#### gs response to PD by phase and treatment ####
# test plot without outlier removal
gst |> 
  ggplot() +
  geom_point(
    aes(x = predawn,
        y = gs,
        color = phase),
    size = 1.5,
    alpha = 0.25) +
  # facet_wrap(~phase) +
  scale_x_reverse()

wp_delta |> 
  ggplot() +
  geom_point(aes(x = date_col, 
                 y= delta_psi,
                 col = trt_s))+
  facet_wrap(~trt_s)

gst_remove_outlier |> 
  ggplot() +
  geom_point(
             aes(x = predawn,
                 y = gs),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbarh(data = gst_sum, 
                 aes(y = gs_m,
                     xmin = pd_m - pd_sd,
                     xmax = pd_m + pd_sd),
                 alpha = 0.5) +
  geom_errorbar(data = gst_sum, 
                aes(x = pd_m,
                    ymin = gs_m - gs_sd,
                    ymax = gs_m + gs_sd),
                alpha = 0.5) +
  geom_point(data = gst_sum,
             aes(x = pd_m,
                 y = gs_m),
             size = 3) +
  scale_y_continuous(expression(paste(g[s], " (mmol ", H[2], "O ", m^-2, " ", s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[PD], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_grid2(rows = vars(trt_label),
              cols = vars(phase),
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside")

#### ET response to SWP by phase and treatment ####
ggplot() +
  geom_point(data = gpp,
             aes(x = swp_0_10_mean,
                 y = ET),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum, 
                aes(x = swp_0_10_mean,
                    ymin = et_m - et_sd,
                    ymax = et_m + et_sd),
                alpha = 0.5) +
  geom_point(data = gpp_sum,
             aes(x = swp_0_10_mean,
                 y = et_m),
             size = 3) +
  scale_y_continuous(expression(paste("ET (mmol ", H[2], O, " ", m^-2, s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_grid2(rows = vars(trt_label),
              cols = vars(phase),
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside") +
  guides(shape = "none",
         lty = "none")

#### GPP response to SWP by phase and treatment ####
ggplot() +
  geom_point(data = gpp,
             aes(x = swp_0_10_mean,
                 y = GPP),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum, 
                aes(x = swp_0_10_mean,
                    ymin = gpp_m - gpp_sd,
                    ymax = gpp_m + gpp_sd),
                alpha = 0.5) +
  geom_point(data = gpp_sum,
             aes(x = swp_0_10_mean,
                 y = gpp_m),
             size = 3) +
  scale_y_continuous(expression(paste("GPP (", mu, "mol ", CO[2], " ", m^-2, s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_grid2(rows = vars(trt_label),
              cols = vars(phase),
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside") +
  guides(shape = "none",
         lty = "none")
