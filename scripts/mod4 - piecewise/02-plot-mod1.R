# Plot parameters for predawn LWP 

library(coda)
library(tidyverse)
library(broom.mixed)

# Load data
load("scripts/mod4 - piecewise/s4_all.Rdata")
temp <- dat2 |> 
  filter(variable == "WP",
         period == "predawn") |> 
  mutate(plot = factor(ID))

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

preds <- cbind.data.frame(temp, pred_sum)

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
  geom_point(aes(x = days_since_pulse, y = value, color = ID)) +
  geom_pointrange(aes(x = days_since_pulse, ymin = pred.lower,
                      ymax = pred.upper,
                      y = pred.mean),
                  alpha = 0.25, color = "gray50") +
  scale_y_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  theme_bw(base_size = 14)


