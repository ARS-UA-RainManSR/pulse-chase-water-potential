# gs, ET, and GPP vs PD or SWP
# faceted by treatment

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggh4x)


#### Load env and wp variables ####
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

# wp_delta <- read_csv("data_clean/wp_rwc_long.csv") |> 
#   select(-time_wp, -time_rwc, -cID, -notes_wp, -notes_rwc, -plant) |> 
#   filter(variable == "WP") |> 
#   pivot_wider(names_from = period, values_from = value) |> 
#   mutate(delta_psi = predawn - midday)

# Psi_soil threshold from fig4.R script
Psi_soil <- -0.917

#### Load fluxes, remove outliers, and calculate summaries ####
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
  # Phase differentiation based on Psi_soil threshold from Fig. 4
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))|>
  left_join(wp_pd, join_by(date_col, trt_s, ID)) |> 
  rename(predawn = value)

# Check high leverage pd values
outlier_phase1 <- gst |> 
  relocate(predawn, .after = gs) |> 
  filter(phase == "Phase 1") |> 
  ungroup() |> 
  arrange(predawn) |> 
  slice_head(n = 1)

outlier_phase2 <- gst |> 
  relocate(predawn, .after = gs) |> 
  filter(phase == "Phase 2") |> 
  ungroup() |> 
  arrange(predawn) |> 
  slice_head(n = 1)

# High leverage point occurs on 8/26 in H1P2, S2 treatment
# Day of minimal difference between predawn and midday (-1.84 vs. -1.91)
# Even as mean SWP is -0.91. Already disconnected from soil?
# Another high leverage point on 8/14, 
# Also minimal difference between predawn and midday (-4.31 vs. -4.95)
# And gs is somehow high. Measurement error or plant disconnected from soil?

gst_remove_outlier <- gst |> 
  anti_join(outlier_phase1) |> 
  anti_join(outlier_phase2)

gst_sum <- gst_remove_outlier |>
  group_by(trt_s, date_col, days_since_pulse) |>
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
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))


# Load ecosystem fluxes, rerun both models, calculate resids
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
  # Phase differentiation based on Psi_soil threshold from Fig. 4
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))

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
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))


#### Gs ####
# How is gs related to predawn water potential during phase 2?

##### Stats #####
# Trim dataframe to Phase 2 only
gs_2 <- gst_remove_outlier |>
  filter(phase == "Phase 2")

# Run model with plot random effects
# Not enough power to run  plant within plot
rem_gs <- lme4::lmer(gs ~ predawn + (1|ID), data = gs_2)
summary(rem_gs)

# Calculate KR approximation of df 
vcovadj <- pbkrtest::vcovAdj(rem_gs)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

gs_fixed_effects <- lme4::fixef(rem_gs)
gs_contrasts <- diag(length(gs_fixed_effects)) # for each row, non-additive

gs_fe_results <- data.frame(parameter = as.character(rep(NA, length(gs_fixed_effects))),
                           estimate = as.numeric(rep(NA, length(gs_fixed_effects))),
                           ci = as.character(rep(NA, length(gs_fixed_effects))),
                           den.df = as.numeric(rep(NA, length(gs_fixed_effects))),
                           tstat = as.numeric(rep(NA, length(gs_fixed_effects))),
                           pval = as.character(rep(NA, length(gs_fixed_effects))))

# Do linear hypothesis tests to isolate the effect of each parameter:
for(r in 1:length(gs_fixed_effects)) {
  contrast.mat <- matrix(gs_contrasts[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(rem_gs, contrast.mat)
  pt.est <- lme4::fixef(rem_gs) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  gs_fe_results[r, c("parameter", "ci", "pval")] <- c(names(gs_fixed_effects)[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  gs_fe_results[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

gs_fe_results

# Predawn is significantly related to gs p < 0.001

##### Plot #####
cols_bl <- brewer.pal(9, "Blues")[c(4,7,9)]
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(3)])))

fig_a <- ggplot() +
  geom_point(data = gst_remove_outlier |>
               filter(phase == "Phase 2"),
             aes(x = predawn,
                 y = gs,
                 col = trt_label,
                 shape = trt_label),
             alpha = 0.5) +
  geom_errorbarh(data = gst_sum |> 
                   filter(phase == "Phase 2"),
                 aes(xmin = pd_m - pd_sd, xmax = pd_m + pd_sd, y = gs_m,
                     color = trt_label),
                 height = 0) +
  geom_errorbar(data = gst_sum |> 
                  filter(phase == "Phase 2"),
                aes(x = pd_m, ymin = gs_m - gs_sd, ymax = gs_m + gs_sd,
                    color = trt_label),
                width = 0) +
  geom_point(data = gst_sum |> 
               filter(phase == "Phase 2"),
             aes(x = pd_m, y = gs_m,
                 color = trt_label,
                 shape = trt_label),
             size = 3) +
  geom_abline(slope = gs_fe_results[2,2],
              intercept = gs_fe_results[1,2]) +
  facet_grid2(~phase,
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  scale_color_manual(values = cols_bl) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste(g[s], " (mmol ", H[2], "O ", m^-2, " ", s^-1, ")"))) +
  # scale_x_reverse() +
  coord_trans(x = "reverse") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.75),
        legend.background = element_blank())

#### ET ####
# How is ET related to soil water potential during phase 2?

##### Stats #####
# Trim dataframe to Phase 2 only
et_2 <- gpp |>
  filter(phase == "Phase 2")

# Run model with plot random effects
# Not enough power to run  plant within plot
rem_et <- lme4::lmer(ET ~ swp_0_10_mean + (1|ID), data = et_2)
summary(rem_et)

# Calculate KR approximation of df 
vcovadj <- pbkrtest::vcovAdj(rem_et)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

et_fixed_effects <- lme4::fixef(rem_et)
et_contrasts <- diag(length(et_fixed_effects)) # for each row, non-additive

et_fe_results <- data.frame(parameter = as.character(rep(NA, length(et_fixed_effects))),
                            estimate = as.numeric(rep(NA, length(et_fixed_effects))),
                            ci = as.character(rep(NA, length(et_fixed_effects))),
                            den.df = as.numeric(rep(NA, length(et_fixed_effects))),
                            tstat = as.numeric(rep(NA, length(et_fixed_effects))),
                            pval = as.character(rep(NA, length(et_fixed_effects))))

# Do linear hypothesis tests to isolate the effect of each parameter:
for(r in 1:length(et_fixed_effects)) {
  contrast.mat <- matrix(et_contrasts[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(rem_et, contrast.mat)
  pt.est <- lme4::fixef(rem_et) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  et_fe_results[r, c("parameter", "ci", "pval")] <- c(names(et_fixed_effects)[r],
                                                      paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                             ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                      ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  et_fe_results[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

et_fe_results

# ET is significantly related to SWP, p = 0.001

##### Plot #####
fig_b <- gpp |>
  filter(phase == "Phase 2") |> 
  ggplot() +
  geom_point(aes(x = swp_0_10_mean,
                 y = ET,
                 col = PT,
                 shape = PT),
             alpha = 0.5) +
  # geom_errorbarh(data = gpp_sum |> 
  #                  filter(phase == "Phase 2"),
  #                aes(xmin = swp_0_10_mean - swp_0_10_sd, 
  #                    xmax = swp_0_10_mean + swp_0_10_sd, 
  #                    y = et_m,
  #                    color = PT),
  #                height = 0) +
  geom_errorbar(data = gpp_sum |> 
                  filter(phase == "Phase 2"),
                aes(x = swp_0_10_mean, 
                    ymin = et_m - et_sd, 
                    ymax = et_m + et_sd,
                    color = PT),
                width = 0) +
  geom_point(data = gpp_sum |> 
               filter(phase == "Phase 2"),
             aes(x = swp_0_10_mean, y = et_m,
                 color = PT,
                 shape = PT),
             size = 3) +
  geom_abline(slope = et_fe_results[2,2],
              intercept = et_fe_results[1,2]) +
  scale_y_continuous(expression(paste("ET (mmol ", H[2], O, " ", m^-2, s^-1, ")"))) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  facet_grid2(~phase,
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  scale_color_manual(values = cols_bl) +
  coord_trans(x = "reverse") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.background = element_blank()) +
  guides(color = "none",
         shape = "none")

#### GPP ####
# How is GPP related to soil water potential during phase 2?

##### Stats #####
# Trim dataframe to Phase 2 only
gpp_2 <- gpp |>
  filter(phase == "Phase 2")

# Run model with plot random effects
# Not enough power to run  plant within plot
rem_gpp <- lme4::lmer(GPP ~ swp_0_10_mean + (1|ID), data = gpp_2)
summary(rem_gpp)

# Calculate KR approximation of df 
vcovadj <- pbkrtest::vcovAdj(rem_gpp)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

gpp_fixed_effects <- lme4::fixef(rem_gpp)
gpp_contrasts <- diag(length(gpp_fixed_effects)) # for each row, non-additive

gpp_fe_results <- data.frame(parameter = as.character(rep(NA, length(gpp_fixed_effects))),
                            estimate = as.numeric(rep(NA, length(gpp_fixed_effects))),
                            ci = as.character(rep(NA, length(gpp_fixed_effects))),
                            den.df = as.numeric(rep(NA, length(gpp_fixed_effects))),
                            tstat = as.numeric(rep(NA, length(gpp_fixed_effects))),
                            pval = as.character(rep(NA, length(gpp_fixed_effects))))

# Do linear hypothesis tests to isolate the effect of each parameter:
for(r in 1:length(gpp_fixed_effects)) {
  contrast.mat <- matrix(gpp_contrasts[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(rem_gpp, contrast.mat)
  pt.est <- lme4::fixef(rem_gpp) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  gpp_fe_results[r, c("parameter", "ci", "pval")] <- c(names(gpp_fixed_effects)[r],
                                                      paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                             ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                      ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  gpp_fe_results[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

gpp_fe_results

# GPP is significantly related to SWP, p < 0.001

##### Plot #####
fig_c <- gpp |>
  filter(phase == "Phase 2") |> 
  ggplot() +
  geom_point(aes(x = swp_0_10_mean,
                 y = GPP,
                 col = PT,
                 shape = PT),
             alpha = 0.5) +
  # geom_errorbarh(data = gpp_sum |> 
  #                  filter(phase == "Phase 2"),
  #                aes(xmin = swp_0_10_mean - swp_0_10_sd, 
  #                    xmax = swp_0_10_mean + swp_0_10_sd, 
  #                    y = gpp_m,
  #                    color = PT),
  #                height = 0) +
  geom_errorbar(data = gpp_sum |> 
                  filter(phase == "Phase 2"),
                aes(x = swp_0_10_mean, 
                    ymin = gpp_m - gpp_sd, 
                    ymax = gpp_m + gpp_sd,
                    color = PT),
                width = 0) +
  geom_point(data = gpp_sum |> 
               filter(phase == "Phase 2"),
             aes(x = swp_0_10_mean, y = gpp_m,
                 color = PT,
                 shape = PT),
             size = 3) +
  geom_abline(slope = gpp_fe_results[2,2],
              intercept = gpp_fe_results[1,2]) +
  scale_y_continuous(expression(paste("GPP (", mu, "mol ", CO[2], " ", m^-2, s^-1, ")"))) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  facet_grid2(~phase,
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  scale_color_manual(values = cols_bl) +
  coord_trans(x = "reverse") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.8, 0.8),
        legend.background = element_blank()) +
  guides(color = "none",
         shape = "none")

#### Assemble panels for fig 7 ####


fig_7 <- cowplot::plot_grid(fig_a, fig_b, fig_c, 
                            nrow = 3,
                            align = "hv", 
                            labels = "auto")
fig_7

ggsave(filename = "fig_scripts/round2/fig7.png",
       plot = fig_7,
       height = 8,
       width = 4,
       units = "in")
