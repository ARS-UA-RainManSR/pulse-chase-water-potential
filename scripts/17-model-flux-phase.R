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

# Psi_soil threshold from fig4.R script
Psi_soil <- -0.917

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
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))

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
  mutate(phase = if_else(swp_0_10_mean > Psi_soil, "Phase 1", "Phase 2"))

#### EDA/model for gs ####
ggplot(gst) +
  geom_point(aes(x = swp_0_10_mean, y = gs))+
  facet_grid(col = vars(phase),
             row = vars(trt_s), 
             scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on gs change by phase, depending on time of day?
gs.swp.phase <- lme4::lmer(gs ~ phase + swp_0_10_mean*phase + (1|ID), data = gst |> 
                             filter(trt_s == "S4"))
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

write_csv(model.fixef.results, "tables/fluxes/gs_SWP_phase_KR_S4.csv")

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
write_csv(model.fixef.results2, "tables/fluxes/gs_SWP_cellmeans_KR_S4.csv")

# Answer: Yes, slope of gs and SWP are significant in phase 2 only for S4 


#### EDA/model for ET ####
ggplot(gpp) +
  geom_point(aes(x = swp_0_10_mean, y = ET))+
  facet_grid(col = vars(phase),
             row = vars(trt_label), 
             scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on ET change by phase, depending on time of day?
et.swp.phase <- lme4::lmer(ET ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp |> 
                             filter(trt_label == "P21"))
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

write_csv(model.fixef.results, "tables/fluxes/ET_SWP_phase_KR_S4.csv")

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
write_csv(model.fixef.results2, "tables/fluxes/ET_SWP_cellmeans_KR_S4.csv")


# Answer: Yes, slope of ET and SWP are significant in both phases
# Though less significantly in phase 2

#### EDA/model for GPP ####
ggplot(gpp) +
  geom_point(aes(x = swp_0_10_mean, y = GPP))+
  facet_grid(col = vars(phase),
             row = vars(trt_label), 
             scale = "free_x") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)")))


# Does the effect of SWP on GPP change by phase, depending on time of day?
gpp.swp.phase <- lme4::lmer(GPP ~ phase + swp_0_10_mean*phase + (1|ID), 
                            data = gpp |> filter(trt_label == "P21"))
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

write_csv(model.fixef.results, "tables/fluxes/GPP_SWP_phase_KR_S4.csv")

# Test an additive version: two slopes and two intercepts
param.names <- c("Phase 1:GPP", "Phase 1:SWP",
                 "Phase 2:GPP", "Phase 2:SWP")
fixed.effects.contrasts3 <- matrix(c(1,0,0,0,
                                     0,0,1,0,
                                     1,1,0,0,
                                     0,0,1,1), ncol = 4, byrow = TRUE)
model.fixef.results3 <- data.frame(parameter = as.character(rep(NA, nrow(fixed.effects.contrasts3))),
                                   estimate = as.numeric(rep(NA, nrow(fixed.effects.contrasts3))),
                                   ci = as.character(rep(NA, nrow(fixed.effects.contrasts3))),
                                   den.df = as.numeric(rep(NA, nrow(fixed.effects.contrasts3))),
                                   tstat = as.numeric(rep(NA, nrow(fixed.effects.contrasts3))),
                                   pval = as.character(rep(NA, nrow(fixed.effects.contrasts3))))
for(r in 1:nrow(fixed.effects.contrasts3)) {
  contrast.mat <- matrix(fixed.effects.contrasts3[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(gpp.swp.phase, contrast.mat)
  pt.est <- lme4::fixef(gpp.swp.phase) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results3[r, c("parameter", "ci", "pval")] <- c(param.names[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results3[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}
write_csv(model.fixef.results3, "tables/fluxes/GPP_SWP_cellmeans_KR_S4.csv")


# Answer: Slope of GPP and SWP are significant in both phases
# but in different directions

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
gs.vpd.phase <- lme4::lmer(resids_gs ~ phase + D_morn_mean*phase + (1|ID), data = gst)
summary(gs.vpd.phase)
vcovadj <- pbkrtest::vcovAdj(gs.swp.phase)
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

#### Explore ET variation with VPD ####

ggplot(gpp) +
  geom_point(aes(x = D_morn_mean, y = resids_et,
                 color = phase))
# Does ET vary with D across phases?
et.vpd.phase <- lme4::lmer(resids_et ~ phase + D_morn_mean*phase + (1|ID), data = gpp)
summary(et.vpd.phase)
vcovadj <- pbkrtest::vcovAdj(et.vpd.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(et.vpd.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(et.vpd.phase, contrast.mat)
  pt.est <- fixef(et.vpd.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/etresid_VPD_phase_KR.csv")

# Test non-additive version: two slopes and two intercepts
param.names <- c("Phase 1:et_resids", "Phase 1:VPD",
                 "Phase 2:et_resids", "Phase 2:VPD")
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
  df <- pbkrtest::get_Lb_ddf(et.vpd.phase, contrast.mat)
  pt.est <- lme4::fixef(et.vpd.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/etresid_VPD_cellmeans_KR.csv")

# Answer: Yes, slopes with VPD are both non-zero and different from each other

#### Explore GPP variation with VPD ####

ggplot(gpp) +
  geom_point(aes(x = D_morn_mean, y = resids_gpp,
                 color = phase))

# Does GPP vary with D across phases?
gpp.vpd.phase <- lme4::lmer(resids_gpp ~ phase + D_morn_mean*phase + (1|ID), data = gpp)
summary(gpp.vpd.phase)
vcovadj <- pbkrtest::vcovAdj(gpp.vpd.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gpp.vpd.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gpp.vpd.phase, contrast.mat)
  pt.est <- fixef(gpp.vpd.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/gppresid_VPD_phase_KR.csv")

# Test non-additive version: two slopes and two intercepts
param.names <- c("Phase 1:gpp_resids", "Phase 1:VPD",
                 "Phase 2:gpp_resids", "Phase 2:VPD")
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
  df <- pbkrtest::get_Lb_ddf(gpp.vpd.phase, contrast.mat)
  pt.est <- lme4::fixef(gpp.vpd.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/gppresid_VPD_cellmeans_KR.csv")

# Answer: No, slopes with VPD are non-significant and not different from each other

#### Explore GPP variation with PAR ####

ggplot(gpp) +
  geom_point(aes(x = Par, y = resids_gpp,
                 color = phase))

# Does GPP vary with PAR across phases?
gpp.par.phase <- lme4::lmer(resids_gpp ~ phase + Par*phase + (1|ID), data = gpp)
summary(gpp.par.phase)
vcovadj <- pbkrtest::vcovAdj(gpp.par.phase)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects <- lme4::fixef(gpp.par.phase)
fixed.effects.contrasts <- diag(length(model.fixed.effects))
model.fixef.results <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects))),
                                  estimate = as.numeric(rep(NA, length(model.fixed.effects))),
                                  ci = as.character(rep(NA, length(model.fixed.effects))),
                                  den.df = as.numeric(rep(NA, length(model.fixed.effects))),
                                  tstat = as.numeric(rep(NA, length(model.fixed.effects))),
                                  pval = as.character(rep(NA, length(model.fixed.effects))))

for (r in 1:length(model.fixed.effects)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(gpp.par.phase, contrast.mat)
  pt.est <- fixef(gpp.par.phase) %*% t(contrast.mat)
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

write_csv(model.fixef.results, "tables/fluxes/gppresid_PAR_phase_KR.csv")

# Test non-additive version: two slopes and two intercepts
param.names <- c("Phase 1:gpp_resids", "Phase 1:PAR",
                 "Phase 2:gpp_resids", "Phase 2:PAR")
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
  df <- pbkrtest::get_Lb_ddf(gpp.par.phase, contrast.mat)
  pt.est <- lme4::fixef(gpp.par.phase) %*% t(contrast.mat)
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
write_csv(model.fixef.results2, "tables/fluxes/gppresid_PAR_cellmeans_KR.csv")

# Answer: No, slopes with PAR are non-significant and not different from each other
