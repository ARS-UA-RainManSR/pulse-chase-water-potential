# alternate Figure 5

# Uses threshold of SWP = -1 MPa
# And combine data from all treatments

library(coda)
library(tidyverse)
library(broom.mixed)
library(RColorBrewer)
library(broom)
library(ggh4x)
library(cowplot)

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
# Does the effect of SWP change by phase, depending on time of day?
mr2 <- lme4::lmer(value ~ period2 + SWP_1*phase + (1|ID), data = wp_all)
summary(mr2)
mm2 <- nlme::lme(value ~ Time + SWP*Phase, random = ~1|ID, data = wp_all)
summary(mm2)
coef(mm2)
anova(mm2) # interaction between Dmean:phase is not significant
# Only use significant parameters
tm2 <- broom::tidy(mm2) |>
  filter(effect == "fixed")

params2 <- data.frame(period2 = rep(c("PD", "MD"), 2),
                      phase = rep(c("Phase 1", "Phase 2"), each = 2),
                      ints = c(tm2$estimate[1],
                               tm2$estimate[1] + tm2$estimate[2],
                               tm2$estimate[1] + tm2$estimate[4],
                               tm2$estimate[1] + tm2$estimate[2] + tm2$estimate[4]),
                      slopes = rep(c(tm2$estimate[3],
                                     tm2$estimate[3] + tm2$estimate[5]), each = 2),
                      sig = c(F, F, T, T))

lab2 <- params2 |>
  group_by(phase, sig) |>
  summarize(slope = unique(slopes)) |>
  mutate(label = case_when(sig == TRUE ~ paste0("Slope: ", round(slope, 3)),
                           sig == FALSE ~ ""))

# Do the residuals of the SWP model (mm2) vary by VPD?
wp_all$swp_resids <- resid(mm2)

wp_all |>
  ggplot(aes(x = VPD, y = swp_resids)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~phase)

mm3 <- nlme::lme(swp_resids ~ Time*VPD + VPD*Phase + Time*Phase, random = ~1|ID, data = wp_all)
summary(mm3)
coef(mm3)
anova(mm3) # interaction between Dmean:phase is not significant

# Only use significant parameters
tm3 <- broom::tidy(mm3) |>
  filter(effect == "fixed") |>
  mutate(sig = ifelse(p.value < 0.05, TRUE, FALSE),
         type = ifelse(grepl("VPD", term), "slope", "int")) |>
  separate(term, into = c("term1", "term2"), sep = ":") |>
  relocate(type, .after = term2)


get_slope <- function(period2, phase_num) {
  if(period2 == "PD" & phase_num == 1) {
    tm3 |>
      filter(type == "slope",
             term1 == "VPD" & is.na(term2)) |>
      pull(estimate) |>
      sum()
  } else if(period2 == "MD" & phase_num == 1) {
    tm3 |>
      filter(type == "slope",
             is.na(term2)|
               grepl("MD", term1) &
               !grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else if(period2 == "PD" & phase_num == 2) {
    tm3 |>
      filter(type == "slope",
             is.na(term2)|
               !grepl("MD", term1) &
               grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else {
    tm3 |>
      filter(type == "slope") |>
      pull(estimate) |>
      sum()
  }

}

get_int <- function(period2, phase_num) {
  if(period2 == "PD" & phase_num == 1) {
    tm3 |>
      filter(type == "int",
             term1 == "(Intercept)" & is.na(term2)) |>
      pull(estimate) |>
      sum()
  } else if(period2 == "MD" & phase_num == 1) {
    tm3 |>
      filter(type == "int",
             term1 == "(Intercept)" |
              grepl("MD", term1) &
               !grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else if(period2 == "PD" & phase_num == 2) {
    tm3 |>
      filter(type == "int",
             term1 == "(Intercept)" |
              grepl("2", term1))|>
      pull(estimate) |>
      sum()
  } else {
    tm3 |>
      filter(type == "int") |>
      pull(estimate) |>
      sum()
  }
}
get_int_v <- Vectorize(get_int)
get_slope_v <- Vectorize(get_slope)

# Make dataframe for plotting
params3 <- data.frame(period2 = rep(c("PD", "MD"), 2),
                      phase = rep(c("Phase 1", "Phase 2"), each = 2),
                      phase_num = rep(1:2, each = 2)) |>
  mutate(ints = get_int_v(period2, phase_num),
         slope = get_slope_v(period2, phase_num))

ggplot() +
  geom_point(data = wp_all,
             aes(x = VPD, 
                 y = swp_resids, # value
                 color = period2)) +
  geom_abline(data = params3,
              aes(slope = slope,
                  intercept = ints,
                  color = period2)) +
  facet_wrap(~phase)


#### Trying out more complex model for SWP ####

mm4 <- nlme::lme(value ~ Time*SWP + SWP*Phase + Time*Phase, random = ~1|ID, data = wp_all)
summary(mm4)
coef(mm4)
anova(mm4) # interaction between Dmean:phase is not significant

# Only use significant parameters
tm4 <- broom::tidy(mm4) |>
  filter(effect == "fixed") |>
  mutate(sig = ifelse(p.value < 0.05, TRUE, FALSE),
         type = ifelse(grepl("SWP", term), "slope", "int")) |>
  separate(term, into = c("term1", "term2"), sep = ":") |>
  relocate(type, .after = term2)


get_slope <- function(period2, phase_num) {
  if(period2 == "PD" & phase_num == 1) {
    tm4 |>
      filter(type == "slope",
             term1 == "SWP" & is.na(term2)) |>
      pull(estimate) |>
      sum()
  } else if(period2 == "MD" & phase_num == 1) {
    tm4 |>
      filter(type == "slope",
             is.na(term2)|
               grepl("MD", term1) &
               !grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else if(period2 == "PD" & phase_num == 2) {
    tm4 |>
      filter(type == "slope",
             is.na(term2)|
               !grepl("MD", term1) &
               grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else {
    tm4 |>
      filter(type == "slope") |>
      pull(estimate) |>
      sum()
  }
  
}

get_int <- function(period2, phase_num) {
  if(period2 == "PD" & phase_num == 1) {
    tm4 |>
      filter(type == "int",
             term1 == "(Intercept)" & is.na(term2)) |>
      pull(estimate) |>
      sum()
  } else if(period2 == "MD" & phase_num == 1) {
    tm4 |>
      filter(type == "int",
             term1 == "(Intercept)" |
               grepl("MD", term1) &
               !grepl("2", term2))|>
      pull(estimate) |>
      sum()
  } else if(period2 == "PD" & phase_num == 2) {
    tm4 |>
      filter(type == "int",
             term1 == "(Intercept)" |
               grepl("2", term1))|>
      pull(estimate) |>
      sum()
  } else {
    tm4 |>
      filter(type == "int") |>
      pull(estimate) |>
      sum()
  }
}
get_int_v <- Vectorize(get_int)
get_slope_v <- Vectorize(get_slope)

# Make dataframe for plotting
params4 <- data.frame(period2 = rep(c("PD", "MD"), 2),
                      phase = rep(c("Phase 1", "Phase 2"), each = 2),
                      phase_num = rep(1:2, each = 2)) |>
  mutate(ints = get_int_v(period2, phase_num),
         slope = get_slope_v(period2, phase_num))

ggplot() +
  geom_point(data = wp_all,
             aes(x = SWP, 
                 y = value,
                 color = period2)) +
  geom_abline(data = params4,
              aes(slope = slope,
                  intercept = ints,
                  color = period2)) +
  facet_wrap(~phase, scales = "free_x")
