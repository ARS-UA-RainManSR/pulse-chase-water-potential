# Model effect of D and SWP on LWP
# extract the marginal effects of D and phase

library(tidyverse)
library(emmeans)
library(broom)
library(nlme)
library(lme4)
library(pbkrtest)

# Load S1, S2, and S4 LWP data
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

# Psi_soil threshold from fig4.R script
Psi_soil <- -0.917

# Combine data and classify into phases
wp_all <- wp |> 
  left_join(swp, by = join_by(date_col == date,
                              trt_s)) |> 
  left_join(vpd, by = join_by(date_col == date, 
                              period)) |>
  mutate(phase = case_when(SWP_1 >= Psi_soil ~ "Phase 1",
                           SWP_1 < Psi_soil ~ "Phase 2"),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")))

# Make a test plot
wp_all |> 
  filter(trt_s == "S4") |> 
  ggplot(aes(x = SWP_1, y = value)) +
  geom_point(aes(color = as.factor(days_since_pulse))) +
  facet_wrap(~phase,
             scales = "free_x") +
  scale_x_reverse()

#### Develop models ####

# Does the effect of VPD change by phase, depending on time of day?
m1 <- lm(value ~ period2 + Dmean*phase, data = wp_all)
summary(m1) # R2 = 0.6854
# Only use significant parameters
tm1 <- broom::tidy(m1) |>
  filter(p.value < 0.05)

params1 <- data.frame(period2 = rep(c("PD", "MD"), 2),
                      phase = rep(c("Phase 1", "Phase 2"), each = 2),
                      ints = c(tm1$estimate[1],
                               tm1$estimate[1] + tm1$estimate[2],
                               tm1$estimate[1] + tm1$estimate[4],
                               tm1$estimate[1] + tm1$estimate[2] + tm1$estimate[4]),
                      slopes = rep(tm1$estimate[3], 4))

# Extract marginal means
EMM1 <- emmeans(m1, ~ Dmean*phase)
pairs(EMM1, simple = "phase")

# Does the effect of SWP change by phase, depending on time of day?
m2 <- lm(value ~ period2 + SWP_1*phase, data = wp_all)
summary(m2)
# R2 = 0.7731

tm2 <- broom::tidy(m2) |>
  filter(p.value < 0.05)

params2 <- data.frame(period2 = rep(c("PD", "MD"), 2),
                      phase = rep(c("Phase 1", "Phase 2"), each = 2),
                      ints = c(tm2$estimate[1],
                               tm2$estimate[1] + tm2$estimate[2],
                               tm2$estimate[1] + tm2$estimate[3],
                               tm2$estimate[1] + tm2$estimate[2] + tm2$estimate[3]),
                      slopes = c(0, 0,
                                 rep(tm2$estimate[4], 2)))


# Extract marginal means
EMM2 <- emmeans(m2, ~ SWP_1*phase)
pairs(EMM2)


##### Try RE models #####

# Does the effect of VPD change by phase, depending on time of day?
mm1 <- lme(value ~ period2 + Dmean*phase, random= ~1|ID, data = wp_all)
summary(mm1)
coef(mm1)
plot(ranef(mm1)) # 16 total plots, random intercepts
plot(residuals(mm1))
anova(mm1) # interaction between Dmean:phase is not significant
glance(mm1)
# Only use significant parameters
tm1 <- broom::tidy(mm1) |>
  filter(p.value < 0.05)

# Does the effect of SWP change by phase, depending on time of day?
mm2 <- lme(value ~ period2 + SWP_1*phase, random = ~1|ID, data = wp_all)
summary(mm2)
coef(mm2)
anova(mm2)
glance(mm2)
# Only use significant parameters
tm2 <- broom::tidy(mm2) |>
  filter(p.value < 0.05)


# Test with Kenward-Roger approximation for degrees of freedom
# Requires using lme4

# Does the effect of VPD change by phase, depending on time of day?
mr1 <- lme4::lmer(value ~ period2 + Dmean*phase + (1|ID), data = wp_all)
summary(mr1)
vcovadj <- vcovAdj(mr1)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects1 <- lme4::fixef(mr1)
fixed.effects.contrasts <- diag(length(model.fixed.effects1))
model.fixef.results1 <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects1))),
                                   estimate = as.numeric(rep(NA, length(model.fixed.effects1))),
                                   ci = as.character(rep(NA, length(model.fixed.effects1))),
                                   den.df = as.numeric(rep(NA, length(model.fixed.effects1))),
                                   tstat = as.numeric(rep(NA, length(model.fixed.effects1))),
                                   pval = as.character(rep(NA, length(model.fixed.effects1))))
# Do linear hypothesis tests to isolate the effect of each parameter:
for (r in 1:length(model.fixed.effects1)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(mr1, contrast.mat)
  pt.est <- fixef(mr1) %*% t(contrast.mat)
  vcov.est <- contrast.mat %*% vcovadj %*% t(contrast.mat)
  sd.est <- sqrt(vcov.est)
  t.stat <- sqrt(as.numeric(pt.est %*% solve(vcov.est) %*% t(pt.est)))
  t.stat <- ifelse(pt.est < 0, -1*t.stat, t.stat)
  p.val <- 2*pt(abs(t.stat), df, lower.tail = F)
  model.fixef.results1[r, c("parameter", "ci", "pval")] <- c(names(model.fixed.effects1)[r],
                                                            paste0("(", round(pt.est - sd.est*qt(1 - alpha/2, df, lower.tail = T), digits),
                                                                      ", ", round(pt.est + sd.est*qt(1 - alpha/2, df, lower.tail = T), digits), ")"),
                                                            ifelse(round(p.val, pval.digits) == 0, p.val.ub, round(p.val, pval.digits)))
  model.fixef.results1[r, c("estimate", "den.df", "tstat")] <- round(c(pt.est, df, t.stat), digits)
}

write_csv(model.fixef.results1, "tables/VPD_phase_KR.csv")

# Does the effect of SWP change by phase, depending on time of day?
mr2 <- lme4::lmer(value ~ period2 + SWP_1*phase + (1|ID), data = wp_all)
summary(mr2)
vcovadj <- vcovAdj(mr2)
alpha <- 0.05
digits <- 3
pval.digits <- 3
p.val.ub <- 0.001

model.fixed.effects2 <- lme4::fixef(mr2)
fixed.effects.contrasts <- diag(length(model.fixed.effects2))
model.fixef.results2 <- data.frame(parameter = as.character(rep(NA, length(model.fixed.effects2))),
                                   estimate = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   ci = as.character(rep(NA, length(model.fixed.effects2))),
                                   den.df = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   tstat = as.numeric(rep(NA, length(model.fixed.effects2))),
                                   pval = as.character(rep(NA, length(model.fixed.effects2))))
# Do linear hypothesis tests to isolate the effect of each parameter:
for (r in 1:length(model.fixed.effects2)) {
  contrast.mat <- matrix(fixed.effects.contrasts[r, ], nrow = 1)
  df <- get_Lb_ddf(mr2, contrast.mat)
  pt.est <- fixef(mr2) %*% t(contrast.mat)
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

write_csv(model.fixef.results2, "tables/SWP_phase_KR.csv")
