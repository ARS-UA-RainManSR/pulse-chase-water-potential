# Script to examine model parameters

library(tidyverse)
library(coda)
library(broom.mixed)
library(cowplot)

# Load data
# indices from hyperspectral
hyp_ind <- read_csv("data_clean/hyp_indices.csv") |> 
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix"))

# Load SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(hyp_ind$date_col),
         date <= max(hyp_ind$date_col),
         depth == "0-12 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         SWP_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)


# Load rwc-wp data, pivot wider, 
# join with hyperspec and swp data
# define phases
hyp_wide <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |>
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  pivot_wider(names_from = variable,
              values_from = value) |>
  # Join hyperspectral
  left_join(hyp_ind) |>
  # Join SWP
  left_join(swp, by = join_by("date_col" == "date", "trt_s")) |>
  # dummy vector to connect the points
  # label with phase determined by SWP < -1 MPa
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2),
         phase = if_else(SWP_1 <= -1, "Phase 2", "Phase 1")) |>
  group_by(phase) |>
  mutate(RWC_ind_scale = scale(RWC_ind))


dat <- hyp_wide |>
  filter(phase == "Phase 2",
         !is.na(RWC_ind)) |> # 76 observations of RWC_ind and WP, but 61 observations of RWC
  select(-ID_long:-WPI2, -CNDI) |>
  mutate(logitRWC = log(RWC/(1-RWC)))

# Load coda
load("scripts/mod6 - rwc/coda/coda_params_rwc.Rdata")

param_sum <- tidyMCMC(coda_params,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

##### Plot RWC vs. RWC_ind ####
# Create beta parameters
B_df <- param_sum |>
  filter(grepl("^B", term)) |>
  mutate(var = c("Intercept", "Slope")) |>
  select(var, pred.mean) |>
  pivot_wider(names_from = var, 
              values_from = pred.mean) |>
  mutate(phase = "Phase 2")

# 32 missing RWC overall, 15 missing from Phase 2
fig_b <-
  ggplot(hyp_wide, aes(x = RWC_ind_scale, y = log(RWC/(1-RWC)))) +
  geom_abline(data = B_df,
              aes(slope = Slope,
                  intercept = Intercept)) +
  geom_point(aes(color = period2)) +
  facet_wrap(~phase) +
  scale_x_continuous(expression(paste(RWC[ind], " (scaled)"))) +
  scale_y_continuous("logit(RWC)",
                     limits = c(-1.5, 4)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  guides(color = "none")

##### Plot WP vs. RWC ####
# Create Gardener parameters
ab_df <-
  param_sum |>
  filter(grepl("log\\.a", term) |
           grepl("inv\\.b", term)) |>
  separate(term, into = c("var", "temp", "period")) |>
  mutate(term = paste0(var, ".", temp)) |>
  relocate(term) |>
  select(-var, -temp, -std.error, -pred.lower, -pred.upper) |>
  mutate(period2 = case_when(period == 1 ~ "PD",
                            period == 2 ~ "MD")) |>
  pivot_wider(names_from = term,
              values_from = pred.mean) |>
    mutate(phase = "Phase 2")

# Create predicted line for Phase 2
gard <- function(rwc, tod) {
  parms <- ab_df |>
    filter(period2 == tod)
  
  logWP = -1*parms$inv.b * (log(rwc) - parms$log.a)
  absWP = exp(logWP)
  return(absWP)
}

gard_pred <- data.frame(rwc = seq(min(hyp_wide$RWC, na.rm = T), 1, by = 0.01)) |>
  mutate(PD = gard(rwc, "PD"),
         MD = gard(rwc, "MD")) |>
  pivot_longer(-rwc,
               names_to = "period2",
               values_to = "absWP") |>
  mutate(WP = -1 * absWP,
         phase = "Phase 2")

# 32 missing RWC overall, 15 missing from Phase 2
fig_a <-
  ggplot() +
  geom_line(data = gard_pred,
            aes(x = rwc, y = WP, 
                color = period2)) + 
  geom_point(data = hyp_wide,
             aes(x = RWC, y = WP,
                 color = period2)) +
  facet_wrap(~phase) +
  scale_x_continuous(expression(paste(RWC, " (g", " ", g^-1, ")"))) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  guides(color = "none")

plot_grid(fig_a, fig_b, ncol = 1,
          align = "hv")
