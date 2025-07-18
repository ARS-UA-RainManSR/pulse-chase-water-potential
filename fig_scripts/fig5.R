# alternate Figure 5

# Uses threshold of SWP = -1 MPa to divide phases
# Residuals from SWP model to evaluate sensitivity to VPD

library(coda)
library(tidyverse)
library(broom.mixed)
library(RColorBrewer)
library(broom)
library(ggh4x)
library(cowplot)
library(lme4)

# Load coda_params
load("scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")

# Summarize
param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Obtain posterior summary of changepoints
cps <- param_sum |> 
  filter(grepl("^mu\\.cp", term))
cp1 <- cps[1,]
cp2 <- cps[2,]
# Load S4 data
wp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")

# Load VPD and SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(wp$date_col),
         date <= max(wp$date_col),
         depth == "0-10 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         SWP_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)

vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "inside",
         period %in% c("PD", "MD"),
         !is.na(mean)) |>  
  dplyr::select(-location, -sd) |> 
  mutate(period = case_when(period == "PD" ~ "predawn",
                            period == "MD" ~ "midday")) |> 
  rename(Dmean = mean)

vwc <- read_csv("data_clean/vwc_daily_daytime.csv") |>
  filter(period == "morn",
         date >= min(wp$date_col),
         date <= max(wp$date_col),
         depth == "0-12 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         VWC_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)

# Combine data and classify into phases
wp_all <- wp |> 
  left_join(swp, by = join_by(date_col == date,
                              trt_s)) |> 
  left_join(vpd, by = join_by(date_col == date,
                              period)) |>
  left_join(vwc, by = join_by(date_col == date,
                              trt_s)) |>
  mutate(phase = case_when(SWP_1 >= -1 ~ "Phase 1",
                           SWP_1 < -1 ~ "Phase 2"),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")),
         Time = period2,
         Phase = case_when(phase == "Phase 1" ~ "1",
                           phase == "Phase 2" ~ "2"),
         VPD = Dmean,
         SWP = SWP_1)

#### Fit models ####

# Check random effects 
foo <- wp_all |>
  group_by(date_col, ID, plant) |>
  count() |>
  arrange(n)
head(foo)

# Fix mislabeled plant
wp_all2 <- wp_all |>
  mutate(plant = case_when(date_col == as.Date("2023-08-16") &
                             ID == "H3P6" &
                             period2 == "MD" ~ "D1",
                           .default = plant))
foo2 <- wp_all2 |>
  group_by(date_col, ID, plant) |>
  count() |>
  arrange(n)
head(foo2)

# Does the effect of SWP change by phase, depending on time of day?
# Random effect of plot, random effect of 
mr2 <- lme4::lmer(value ~ Time + SWP*Phase + (plant|ID), data = wp_all2)
summary(mr2)
# mm2 <- nlme::lme(value ~ Time + SWP*Phase, random = ~ plant|ID, data = wp_all2)
# summary(mm2)

# Calculate KR approximation of df 
vcovadj <- pbkrtest::vcovAdj(mr2)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects2 <- lme4::fixef(mr2)
fixed.effects.contrasts2 <- diag(length(model.fixed.effects2)) # for each row, non-additive

model.fixef.results2 <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects2))),
                                   estimate = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   ci = as.character(rep(NA, length(model.fixed.effects2))),
                                   den.df = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   tstat = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   pval = as.character(rep(NA, length(model.fixed.effects2))))
# Do linear hypothesis tests to isolate the effect of each parameter:
for(r in 1:length(model.fixed.effects2)) {
  contrast.mat <- matrix(fixed.effects.contrasts2[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(mr2, contrast.mat)
  pt.est <- lme4::fixef(mr2) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results2[r, c("parameter", "ci", "pval")] <- c(names(model.fixed.effects2)[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results2[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

# Test an additive version: four slopes and two intercepts
param.names <- c("Phase 1:PD", "Phase 1:MD", "Phase 1:SWP",
                 "Phase 2:PD", "Phase 2:MD", "Phase 2:SWP")
fixed.effects.contrasts2b <- matrix(c(1,0,0,0,0,
                                      0,1,0,0,0,
                                      0,0,1,0,0,
                                      1,0,0,1,0,
                                      1,1,0,1,0,
                                      0,0,1,0,1), ncol = 5, byrow = TRUE)
model.fixef.results2b <- data.frame(parameter = as.character(rep(NA, nrow(fixed.effects.contrasts2b))),
                                   estimate = as.numeric(rep(NA, nrow(fixed.effects.contrasts2b))),
                                   ci = as.character(rep(NA, nrow(fixed.effects.contrasts2b))),
                                   den.df = as.numeric(rep(NA, nrow(fixed.effects.contrasts2b))),
                                   tstat = as.numeric(rep(NA, nrow(fixed.effects.contrasts2b))),
                                   pval = as.character(rep(NA, nrow(fixed.effects.contrasts2b))))
for(r in 1:nrow(fixed.effects.contrasts2b)) {
  contrast.mat <- matrix(fixed.effects.contrasts2b[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(mr2, contrast.mat)
  pt.est <- lme4::fixef(mr2) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results2b[r, c("parameter", "ci", "pval")] <- c(param.names[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results2b[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}
write_csv(model.fixef.results2b, "tables/SWP_cellmeans_KR.csv")

# Switch to plotable params
model.fixef.results2b <- read_csv("tables/SWP_cellmeans_KR.csv")

params2 <- model.fixef.results2b |>
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "SWP" ~ "slope",
                          .default = "intercept"),
         term = rep(c("PD", "MD"), 4),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  rename(Time = term) |>
  mutate(Time = factor(Time, levels = c("PD", "MD"))) |>
  select(-ci, -den.df, -tstat, -pval) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)


#### Do the residuals of the SWP model (mm2) vary by VPD? ####
wp_all2$wp_resids <- resid(mr2)

# Fit model
mr3 <- lme4::lmer(wp_resids ~ Time*VPD + VPD*Phase + Time*Phase + (plant|ID), 
                 data = wp_all2)

# Calculate KR approximation of df 
vcovadj <- pbkrtest::vcovAdj(mr3)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects3 <- lme4::fixef(mr3)
fixed.effects.contrasts3 <- diag(length(model.fixed.effects3)) # for each row, non-additive

model.fixef.results3 <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects3))),
                                   estimate = as.numeric(rep(NA, length(model.fixed.effects3))),
                                   ci = as.character(rep(NA, length(model.fixed.effects3))),
                                   den.df = as.numeric(rep(NA, length(model.fixed.effects3))),
                                   tstat = as.numeric(rep(NA, length(model.fixed.effects3))),
                                   pval = as.character(rep(NA, length(model.fixed.effects3))))
# Do linear hypothesis tests to isolate the effect of each parameter:
for(r in 1:length(model.fixed.effects3)) {
  contrast.mat <- matrix(fixed.effects.contrasts3[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(mr3, contrast.mat)
  pt.est <- lme4::fixef(mr3) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results3[r, c("parameter", "ci", "pval")] <- c(names(model.fixed.effects3)[r],
                                                             paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                    ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                             ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results3[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

# Test an additive version: four slopes and two intercepts
param.names <- c("Phase 1:PD", "Phase 1:MD", "Phase 1:PD:VPD", "Phase 1:MD:VPD",
                 "Phase 2:PD", "Phase 2:MD", "Phase 2:PD:VPD", "Phase 2:MD:VPD")
fixed.effects.contrasts3b <- matrix(c(1,0,0,0,0,0,0,
                                      0,1,0,0,0,0,0,
                                      0,0,1,0,0,0,0,
                                      0,0,1,0,1,0,0,
                                      1,0,0,1,0,0,0,
                                      1,1,0,1,0,0,1,
                                      0,0,1,0,0,1,0,
                                      0,0,1,0,1,1,0), ncol = 7, byrow = TRUE)
model.fixef.results3b <- data.frame(parameter = as.character(rep(NA, nrow(fixed.effects.contrasts3b))),
                                    estimate = as.numeric(rep(NA, nrow(fixed.effects.contrasts3b))),
                                    ci = as.character(rep(NA, nrow(fixed.effects.contrasts3b))),
                                    den.df = as.numeric(rep(NA, nrow(fixed.effects.contrasts3b))),
                                    tstat = as.numeric(rep(NA, nrow(fixed.effects.contrasts3b))),
                                    pval = as.character(rep(NA, nrow(fixed.effects.contrasts3b))))
for(r in 1:nrow(fixed.effects.contrasts3b)) {
  contrast.mat <- matrix(fixed.effects.contrasts3b[r, ], nrow = 1)
  df <- pbkrtest::get_Lb_ddf(mr3, contrast.mat)
  pt.est <- lme4::fixef(mr3) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results3b[r, c("parameter", "ci", "pval")] <- c(param.names[r],
                                                              paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                     ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                              ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results3b[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

write_csv(model.fixef.results3b, "tables/VPD_cellmeans_KR.csv")

# Switch to plotable params
model.fixef.results3b <- read_csv("tables/VPD_cellmeans_KR.csv")

params3 <-
  model.fixef.results3b |>
  separate(parameter, into = c("phase", "Time", "type"), sep = ":") |>
  mutate(type = case_when(type == "VPD" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA),
         Time = factor(Time, levels = c("PD", "MD"))) |>
  select(-ci, -den.df, -tstat, -pval) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)

#### Make plot ####

cols_gn <- brewer.pal(4, "Paired")
# display.brewer.pal(4, "Paired")

cols_div <- brewer.pal(7, "Spectral")
# display.brewer.pal(7, "Spectral")

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])))
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(6,3)])))

fig5a <-
  wp_all2 |> 
  ggplot() +
  geom_point(aes(x = SWP, y = value, color = Time)) +
  geom_abline(data = params2,
              aes(slope = -slope, intercept = intercept,
                  color = Time,
                  lty = sig)) +
  # geom_text(data = lab1,
  #           aes(x = -1, y = -5.5, label = label),
  #           parse = TRUE,
  #           hjust = 0) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  scale_color_manual(values = cols_gn[4:3], 
                     label = c("PD", "MD")) +
  scale_linetype_manual(values = c("dashed", "solid")) +  
  facet_wrap2(~phase, strip = strip, scales = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.2),
        legend.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0))),
         linetype = "none")

fig5b <-
  wp_all2 |> 
  ggplot() +
  geom_point(aes(x = VPD, y = wp_resids, color = Time)) +
  geom_abline(data = params3,
              aes(slope = slope, intercept = intercept,
                  color = Time,
                  lty = sig)) +
  # geom_text(data = lab1,
  #           aes(x = -1, y = -5.5, label = label),
  #           parse = TRUE,
  #           hjust = 0) +
  scale_y_continuous(expression(paste(Psi[resids], " (MPa)"))) +
  scale_x_continuous("VPD (kPa)") +
  scale_color_manual(values = cols_gn[4:3]) +
  scale_linetype_manual(values = c("dashed", "solid")) +  
  facet_wrap2(~phase, strip = strip) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.4),
        legend.background = element_blank()) +
  guides(color = "none",
         linetype = "none")

fig5 <- plot_grid(fig5a, fig5b, 
                  ncol = 1,
                  align = "v",
                  labels = "auto")

ggsave(filename = "fig_scripts/round2/fig5.png",
       plot = fig5,
       height = 4.5,
       width = 6,
       units = "in")  

