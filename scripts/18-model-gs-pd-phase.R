# Model gs flux from predawn
library(tidyverse)
library(emmeans)
library(broom)
library(nlme)
library(lme4)
library(pbkrtest)

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
wp_all <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")
wp_pd <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP",
         period == "predawn")

# Load gs
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
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2")) |> 
  left_join(wp_pd, join_by(date_col, trt_s, ID)) |> 
  rename(predawn = value)

#### EDA/model for gs ####
ggplot(gst) +
  geom_point(aes(x = predawn, y = gs))+
  facet_wrap(~phase, scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[PD], " (MPa)")))

# Check high leverage pd values
outlier <- gst |> 
  relocate(predawn, .after = gs) |> 
  filter(phase == "Phase 1") |> 
  ungroup() |> 
  arrange(predawn) |> 
  slice_head(n = 1)

wp_all |> filter(period == "midday", ID == "H1P2") |> 
  relocate(value, .after = plant)
# High leverage point occurs on 8/26 in H1P2, S2 treatment
# Day of minimal difference between predawn and midday (-1.84 vs. -1.91)
# Even as mean SWP is -0.91. Already disconnected from soil?

wp_wide <- wp_all |> pivot_wider(id_cols = date_col:trt, names_from = period, values_from = value) |> 
  mutate(delta = predawn - midday)
ggplot(wp_wide) +
  geom_point(aes(x = trt,
                 y = delta, 
                 color = as.factor(date_col)))
# outlier was only point with basically delta = 0, across all treatments and dates

gst_remove_outlier <- gst |> 
  anti_join(outlier)
  
# Below results do not change as a result of that point! Will state clearly in caption

# Does the effect of predawns on gs change by phase, depending on time of day?
gs.pd.phase <- lme4::lmer(gs ~ phase + predawn*phase + (1|ID), data = gst_remove_outlier)
summary(gs.pd.phase)
vcovadj <- pbkrtest::vcovAdj(gs.pd.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gs.pd.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gs.pd.phase, contrast.mat)
  pt.est <- fixef(gs.pd.phase) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results[r, c("parameter", "ci", "pval")] <- c(names(model.fixed.effects)[r],
                                                            paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                   ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                            ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

write_csv(model.fixef.results, "tables/fluxes/gs_PD_phase_KR.csv")

# Test an additive version: two slopes and two intercepts
param.names <- c("Phase 1:gs", "Phase 1:PD",
                 "Phase 2:gs", "Phase 2:PD")
fixed.effects.contrasts2 <- matrix(c(1,0,0,0,
                                     0,0,1,0,
                                     1,1,0,0,
                                     0,0,1,1), ncol = 4, byrow = TRUE)
model.fixef.results2 <- data.frame(parameter = as.character(rep(NA, nrow(fixed.effects.contrasts2))),
                                   estimate = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   ci = as.character(rep(NA, nrow(fixed.effects.contrasts2))),
                                   den.df = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   tstat = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   pval = as.character(rep(NA, nrow(fixed.effects.contrasts2))))
for(r in 1:nrow(fixed.effects.contrasts2)) {
  contrast.mat <- matrix(fixed.effects.contrasts2[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(gs.pd.phase, contrast.mat)
  pt.est <- lme4::fixef(gs.pd.phase) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results2[r, c("parameter", "ci", "pval")] <- c(param.names[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results2[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}
write_csv(model.fixef.results2, "tables/fluxes/gs_PD_cellmeans_KR.csv")

# Answer: Yes, slope of gs and PD are significant in phase 1 only
# and slope is not significant in phase 2

#### Calculate residuals from the above model ####

gst_remove_outlier <- gst_remove_outlier |> 
  ungroup() |> 
  mutate(resids_gs = residuals(gs.pd.phase))


#### Explore gs residual variation with VPD ####

ggplot(gst_remove_outlier) +
  geom_point(aes(x = D_morn_mean, y = resids_gs,
                 color = phase))

# Does gs vary with D across phases?
gs.vpd.phase <- lme4::lmer(resids_gs ~ phase + D_morn_mean*phase + (1|ID), data = gst_remove_outlier)
summary(gs.vpd.phase)
vcovadj <- pbkrtest::vcovAdj(gs.pd.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gs.vpd.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gs.vpd.phase, contrast.mat)
  pt.est <- fixef(gs.vpd.phase) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results[r, c("parameter", "ci", "pval")] <- c(names(model.fixed.effects)[r],
                                                            paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                   ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                            ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

write_csv(model.fixef.results, "tables/fluxes/gsresid_VPD_phase_KR.csv")

# Test non-additive version: two slopes and two intercepts
param.names <- c("Phase 1:gs_resids", "Phase 1:VPD",
                 "Phase 2:gs_resids", "Phase 2:VPD")
fixed.effects.contrasts2 <- matrix(c(1,0,0,0,
                                     0,0,1,0,
                                     1,1,0,0,
                                     0,0,1,1), ncol = 4, byrow = TRUE)
model.fixef.results2 <- data.frame(parameter = as.character(rep(NA, nrow(fixed.effects.contrasts2))),
                                   estimate = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   ci = as.character(rep(NA, nrow(fixed.effects.contrasts2))),
                                   den.df = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   tstat = as.numeric(rep(NA, nrow(fixed.effects.contrasts2))),
                                   pval = as.character(rep(NA, nrow(fixed.effects.contrasts2))))
for(r in 1:nrow(fixed.effects.contrasts2)) {
  contrast.mat <- matrix(fixed.effects.contrasts2[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(gs.vpd.phase, contrast.mat)
  pt.est <- lme4::fixef(gs.vpd.phase) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results2[r, c("parameter", "ci", "pval")] <- c(param.names[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results2[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}
write_csv(model.fixef.results2, "tables/fluxes/gsresid_VPD_cellmeans_KR.csv")

# Answer: No, slopes are not different from zero or by phase
