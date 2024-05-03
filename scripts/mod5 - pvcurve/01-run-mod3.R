# Pressure-volume model with TLP determined as changepoint
# between exponential decay + linear
# mod3 includes constraining slope c to be negative
# Only use S4 treatment, remove REs


library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)

# Wrangle data
both <- read_csv("data_clean/wp_rwc_long.csv")
wide <- both |> 
  pivot_wider(names_from = variable, 
              values_from = value) |> 
  filter(!is.na(RWC))

S4_pd <- wide |> 
  filter(period == "predawn",
         trt_s == "S4",
         RWC <= 0.95)

# Quick plot

ggplot(S4_pd) +
  geom_point(aes(x = RWC, y = -1/WP))
ggplot() +
  # geom_line(data = S4_pd,
  #           aes(x = 1 - RWC, y = -1/WP,
  #               color = ID)) +
  geom_point(data = S4_pd,
             aes(x = 1 - RWC, y = -1/WP,
                 color = ID))
  
  # scale_color_gradient(low = "coral", high = "royalblue") +
  # facet_wrap(~trt_s)

# Data list
dat_list <- list(y = -1/S4_pd$WP,
                 irwc = 1-S4_pd$RWC,
                 N = nrow(S4_pd)
                 # plot = factor(S4_pd$ID),
                 # Nplot = length(unique(S4_pd$ID))
                 )
str(dat_list)  

# Inits list
inits <- function() {
  list(a = rnorm(1, 0, 10),
       b = rnorm(1, 0, 5),
       log.c = runif(1, 0, 5),
       cp = runif(1, 0.3, 0.6),
       tau = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

# Compile model
jm <- jags.model("scripts/mod5 - pvcurve/mod3.JAGS",
                 data = dat_list,
                 inits = inits_list,
                 n.chains = 3)
update(jm, 10000)
dic.samples(jm, n.iter = 10000)

# Params to monitor
params <- c("deviance", "Dsum", "R2",
            "a", "b", "log.c", "c", "d", "cp", 
            "tau", "sig", "tlp"
            )
jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 15000, thin = 15)

# Visualize chains
mcmcplot(jm_coda, parms = c("deviance", "Dsum", "R2", "sig",
                            "a", "b", "log.c", "c", "cp", "tlp"))


# Summarize parameters
param_sum <- tidyMCMC(jm_coda, conf.int = TRUE,
                      conf.method = "HPDinterval") |>
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

coef1 <- param_sum |>
  filter(term %in% c("a", "b"))

coef2 <- param_sum |> 
  filter(term %in% c("c", "d"))

cp <- param_sum |> 
  filter(term == "cp")

#### Plot ####
# Plot on PV curve scale 

df_exp <- data.frame(x = seq(0.05, 0.7, by = 0.001)) |>
  mutate(y = coef1$pred.mean[1]*exp(coef1$pred.mean[2]*x))

ggplot() +
  geom_vline(data = cp,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  geom_point(data = S4_pd,
             aes(x = 1 - RWC, y = -1/WP,
                 color = ID)) +
  geom_line(data = df_exp,
            aes(x = x, y = y)) +
  geom_abline(data = coef2,
              aes(slope = -pred.mean[1], intercept = pred.mean[2])) + 
  scale_x_continuous("1 - RWC", 
                     limits = 1-rev(range(S4_pd$RWC))) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())
