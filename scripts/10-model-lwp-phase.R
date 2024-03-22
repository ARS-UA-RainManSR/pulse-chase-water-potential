# Model effect of D and SWP on LWP
# extract the marginal effects of D and phase

library(tidyverse)
library(emmeans)
library(broom)
library(nlme)

# Load S1, S2, and S4 LWP data
wp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")

# Load VPD and SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(wp$date_col),
         date <= max(wp$date_col),
         depth == "0-12 cm",
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

# Combine data and classify into phases
wp_all <- wp |> 
  left_join(swp, by = join_by(date_col == date,
                              trt_s)) |> 
  left_join(vpd, by = join_by(date_col == date, 
                              period)) |>
  mutate(phase = case_when(SWP_1 >= -1 ~ "Phase 1",
                           SWP_1 < -1 ~ "Phase 2"),
         period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")))

#### Develop models ####

# Does the effect of VPD change by phase, depending on time of day?
m1 <- lm(value ~ period2 + Dmean*phase, data = wp_all)
summary(m1) # R2 = 0.7467
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
# R2 = 0.7802

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
summary(mm1) # R2 = 0.7467
coef(mm1)
anova(mm1) # interaction between Dmean:phase is not significant
# Only use significant parameters
tm1 <- broom::tidy(mm1) |>
  filter(p.value < 0.05)

# Does the effect of SWP change by phase, depending on time of day?
mm2 <- lme(value ~ period2 + SWP_1*phase, random= ~1|ID, data = wp_all)
summary(mm2)
# R2 = 0.7802
coef(mm2)
anova(mm2) # interaction between Dmean:phase is not significant

tm2 <- broom::tidy(mm2) |>
  filter(p.value < 0.05)
