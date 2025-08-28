# Model the effect of SWP and phase on fluxes

library(tidyverse)
library(emmeans)
library(broom)
library(nlme)
library(lme4)
library(pbkrtest)

#### Prepare dataframe ####
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
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))

# Load GPP
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
  select(-period)|> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))

#### EDA/model for gs ####
ggplot(gst) +
  geom_point(aes(x = swp_0_10_mean, y = gs))+
  facet_wrap(~phase, scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on gs change by phase, depending on time of day?
gs.swp.phase <- lme4::lmer(gs ~ phase + swp_0_10_mean*phase + (1|ID), data = gst)
summary(gs.swp.phase)
vcovadj <- pbkrtest::vcovAdj(gs.swp.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gs.swp.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                   estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                   ci = as.character(rep(NA, length(model.fixed.effects))),
                                   den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                   tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                   pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gs.swp.phase, contrast.mat)
  pt.est <- fixef(gs.swp.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/gs_SWP_phase_KR.csv")

# Test an additive version: two slopes and two intercepts
param.names <- c("Phase 1:gs", "Phase 1:SWP",
                 "Phase 2:gs", "Phase 2:SWP")
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
  df <- pbkrtest::get_Lb_ddf(gs.swp.phase, contrast.mat)
  pt.est <- lme4::fixef(gs.swp.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/gs_SWP_cellmeans_KR.csv")

# Answer: Yes, slope of gs and SWP are significant in phase 1 
# and slope is significantly different in phase 2

#### EDA/model for ET ####
ggplot(gpp) +
  geom_point(aes(x = swp_0_10_mean, y = ET))+
  facet_wrap(~phase, scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on ET change by phase, depending on time of day?
et.swp.phase <- lme4::lmer(ET ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)
summary(et.swp.phase)
vcovadj <- pbkrtest::vcovAdj(et.swp.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(et.swp.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(et.swp.phase, contrast.mat)
  pt.est <- fixef(et.swp.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/ET_SWP_phase_KR.csv")

# Test an additive version: two slopes and two intercepts
param.names <- c("Phase 1:ET", "Phase 1:SWP",
                 "Phase 2:ET", "Phase 2:SWP")
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
  df <- pbkrtest::get_Lb_ddf(et.swp.phase, contrast.mat)
  pt.est <- lme4::fixef(et.swp.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/ET_SWP_cellmeans_KR.csv")


# Answer: Yes, slope of ET and SWP are significant in phase 1 
# but not in phase 2

#### EDA/model for GPP ####
ggplot(gpp) +
  geom_point(aes(x = swp_0_10_mean, y = GPP))+
  facet_wrap(~phase, scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on GPP change by phase, depending on time of day?
gpp.swp.phase <- lme4::lmer(GPP ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)
summary(gpp.swp.phase)
vcovadj <- pbkrtest::vcovAdj(gpp.swp.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gpp.swp.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gpp.swp.phase, contrast.mat)
  pt.est <- fixef(gpp.swp.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/GPP_SWP_phase_KR.csv")

# Test an additive version: two slopes and two intercepts
param.names <- c("Phase 1:GPP", "Phase 1:SWP",
                 "Phase 2:GPP", "Phase 2:SWP")
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
  df <- pbkrtest::get_Lb_ddf(gpp.swp.phase, contrast.mat)
  pt.est <- lme4::fixef(gpp.swp.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/GPP_SWP_cellmeans_KR.csv")


# Answer: Yes-ish, slope of GPP and SWP are significant in phase 1 
# but the phase 2 slope is only marginally different (p = 0.052)

# Plot above slopes with Figure 6-7


#### Calculate residuals from above models ####

gst <- gst |> 
  ungroup() |> 
  mutate(resids_gs = residuals(gs.swp.phase))

gpp <- gpp |> 
  ungroup() |> 
  mutate(resids_et = residuals(et.swp.phase),
         resids_gpp = residuals(gpp.swp.phase))

#### Explore gs residual variation with VPD ####

ggplot(gst) +
  geom_point(aes(x = D_morn_mean, y = resids_gs,
                 color = phase))
# Does gs vary with D across phases?
m1 <- lm(resids_gs ~ D_morn_mean*phase -1, data = gst)
summary(m1)
# Neither by D or by phase or their interaction
em <- emtrends(m1, "phase", var = "D_morn_mean")
# Are the slopes of each phas different from each other?
pairs(emtrends(m1, "phase", var = "D_morn_mean")) # No

# Make an output table
param.names <- c("Phase 1:gs", "Phase 1:VPD",
                 "Phase 2:gs", "Phase 2:VPD")
df <- 120
alpha <- 0.05

cell_means <- data.frame(parameter = param.names,
                         estimate = c(coef(m1)[2], 
                                      summary(em)$D_morn_mean.trend[1],
                                      coef(m1)[3],
                                      summary(em)$D_morn_mean.trend[2]),
                         SE = c(summary(m1)$coef[2,2],
                                summary(em)$SE[1],
                                summary(m1)$coef[2,3],
                                summary(em)$SE[2]))
                         
write_csv(cell_means, "tables/fluxes/gs_VPD_cellmeans_lm.csv")

#### Explore ET variation with VPD ####

ggplot(gpp) +
  geom_point(aes(x = D_morn_mean, y = resids_et,
                 color = phase))
# Does ET vary with D across phases?
m1 <- lm(resids_et ~ D_morn_mean*phase -1, data = gpp)
summary(m1)

# By phase 1 and phase 2 ET respond differently to D
# Also different intercepts
# Neither by D or by phase or their interaction
em <- emtrends(m1, "phase", var = "D_morn_mean")
# Are the slopes of each phas different from each other?
pairs(emtrends(m1, "phase", var = "D_morn_mean")) # No

# Make an output table
param.names <- c("Phase 1:ET", "Phase 1:VPD",
                 "Phase 2:ET", "Phase 2:VPD")

cell_means <- data.frame(parameter = param.names,
                         estimate = c(coef(m1)[2], 
                                      summary(em)$D_morn_mean.trend[1],
                                      coef(m1)[3],
                                      summary(em)$D_morn_mean.trend[2]),
                         SE = c(summary(m1)$coef[2,2],
                                summary(em)$SE[1],
                                summary(m1)$coef[2,3],
                                summary(em)$SE[2]))

write_csv(cell_means, "tables/fluxes/ET_VPD_cellmeans_lm.csv")

#### Explore GPP variation with VPD ####

ggplot(gpp) +
  geom_point(aes(x = D_morn_mean, y = resids_gpp,
                 color = phase))
# Does GPP vary with D across phases?
m3 <- lm(resids_gpp ~ D_morn_mean*phase, data = gpp)
summary(m3)
# Nothing significant

#### Explore GPP variation with PAR ####

ggplot(gpp) +
  geom_point(aes(x = Par, y = resids_gpp,
                 color = phase))
# Does GPP vary with D across phases?
m4 <- lm(resids_gpp ~ Par*phase, data = gpp)
summary(m4)
# Nothing significant


