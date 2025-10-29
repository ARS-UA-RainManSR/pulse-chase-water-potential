# Fig S5
# Model fit for double changepoint model

library(coda)
library(broom.mixed)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

#### A) Predicted vs Observed with WP4 samples ####

# Load posterior predicted values
load("scripts/mod4 - piecewise/coda/coda_pred_mod1.Rdata")

sum_pred <- broom.mixed::tidyMCMC(coda_pred, 
                                  conf.int = TRUE, 
                                  conf.method = "HPDinterval") |> 
  rename(pred.upper = conf.high, 
         pred.lower = conf.low, 
         pred.mean = estimate)

# Read in original data and add values
load("scripts/mod4 - piecewise/s4_all.Rdata") # dat2

S4_pd <- dat2 |> 
  filter(variable == "WP",
         period == "predawn") |> 
  bind_cols(sum_pred[, c(2, 4:5)]) |> 
  mutate(plot = factor(ID),
         coverage = ifelse(value <= pred.upper & value >= pred.lower, 1, 0))

m1 <- lm(pred.mean ~ value, data = S4_pd)
summary(m1) # R2 = 0.9272
coefs <- coef(summary(m1))
coefs[2,1] # Bias of 0.897
mean(S4_pd$coverage) # Coverage of 0.981


figS4 <- S4_pd |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = coefs[2,1], intercept = coefs[1,1],
              linetype = "dashed") +
  geom_errorbar(aes(x = value,
                    ymin = pred.lower,
                    ymax = pred.upper),
                width = 0,
                alpha = 0.25) +
  geom_point(aes(x = value, 
                 y = pred.mean)) +
  geom_text(x = -1, y = -4.5, 
            label = "italic(R^2)==0.927",
            parse = TRUE,
            hjust = 0.5,
            vjust = 1,
            size = 4) +
  scale_x_continuous(expression(paste("Observed ", Psi[PD], " (MPa)")),
                     limits = c(-5, 0.405)) +
  scale_y_continuous(expression(paste("Predicted ", Psi[PD], " (MPa)")),
                     limits = c(-5, 0.405)) +
  theme_bw(base_size = 14) +
  coord_equal() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank())

ggsave("fig_scripts/round2/figS5.png",
       figS4,
       height = 4,
       width = 4,
       units = "in")
