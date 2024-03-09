# Plot parameters for predawn LWP 

library(coda)
library(tidyverse)
library(broom.mixed)
library(ggh4x)

# Load data
load("scripts/mod4 - piecewise/s4_all.Rdata")
temp <- dat2 |> 
  filter(variable == "WP",
         period == "predawn") |> 
  mutate(plot = factor(ID))
# n=54 S4 predawns

swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         summer == "S4",
         depth == "0-12 cm") |> 
  select(-period)

# Load coda_params
load("scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")

param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Load coda_pred
load("scripts/mod4 - piecewise/coda/coda_pred_mod1.Rdata")

pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(temp, pred_sum) |> 
  left_join(swp, by = join_by("date_col" == "date")) |> 
  rename(SWP_1 = mean)

cps <- param_sum |> 
  filter(grepl("^mu\\.cp", term))

maxy <- param_sum |> 
  filter(grepl("mu\\.maxy", term))

ab1 <- param_sum |> 
  filter(grepl("mu\\.a$", term) | grepl("mu\\.b\\[1", term)) |> 
  mutate(var = c("intercept", "slope"))

ab2 <- param_sum |> 
  filter(grepl("mu\\.a2", term) | grepl("mu\\.b\\[2", term)) |> 
  mutate(var = c("intercept", "slope"))

preds |> 
  ggplot() +
  geom_hline(data = maxy, 
             aes(yintercept = pred.mean),
             linetype = 2) +
  geom_rect(data = cps,
            aes(ymin = -Inf, ymax = Inf,
                xmin = pred.lower, xmax = pred.upper),
            color = "gray90", alpha = 0.15) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean)) +
  geom_abline(slope = ab1$pred.mean[2],
              intercept = ab1$pred.mean[1],
              linetype = 2) +
  geom_abline(slope = ab2$pred.mean[2],
              intercept = ab2$pred.mean[1],
              linetype = 2) +
  geom_point(aes(x = days_since_pulse, y = value,
                 color = "LWP")) +
  geom_line(aes(x = days_since_pulse, y = SWP_1,
                color = "SWP")) +
  scale_y_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_x_continuous(name = "Day since pulse", 
                     minor_breaks = seq(0, 21, 1),
                     breaks = seq(0, 21, 3),
                     limits = c(0, 21),
                     guide = "axis_minor") +
  theme_bw(base_size = 14) +
  theme(ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid = element_blank())


