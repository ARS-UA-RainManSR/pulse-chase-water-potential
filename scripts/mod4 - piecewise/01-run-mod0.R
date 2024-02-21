# hjierarchical model for 1 changepoint - test

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)

dat <- read_csv("data_clean/wp_rwc_long.csv")

# plot

dat |>
  filter(trt_s == "S4") |> 
  ggplot(aes(x = days_since_pulse, y = value)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  facet_grid(rows = vars(variable),
             cols = vars(period),
             scales = "free_y")

# data list for predawns only
temp <- dat |> 
  filter(trt_s == "S4",
         variable == "WP",
         period == "predawn") |> 
  mutate(plot = factor(ID))
data_list <- list(y = temp$value,
                  dop = temp$days_since_pulse,
                  plot = temp$plot,
                  N = nrow(temp),
                  Nplot = length(unique(temp$plot)))
str(data_list)

# Function for starting values
inits <- function() {
  list(mu.a = rnorm(1, 0, 10),
       mu.b = rnorm(2, 0, 10),
       mu.cp = runif(1, 0.5, 3),
       tau = runif(1, 0, 1),
       sig.a = runif(1, 0, 10),
       sig.b = runif(2, 0, 10),
       sig.cp = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

# Compile model
jm <- jags.model(file = "scripts/mod4 - piecewise/mod0.JAGS",
                 data = data_list,
                 inits = inits_list,
                 n.chains = 3)
update(jm, 10000)
dic.samples(jm, 10000)

# Sample 
params_vec <- c("deviance", "Dsum", "R2",
                "mu.a", "a", "mu.b", "b", "mu.cp", "cp",
                "sig.a", "sig.b", "sig.cp", "sig", "tau")
coda_params <- coda.samples(jm, variable.names = params_vec,
                            n.iter = 10000,
                            thin = 10)

# View posterior chains
mcmcplot(coda_params, parms = c("a", "b", "cp"))
mcmcplot(coda_params, parms = c("deviance", "Dsum", "R2",
                                "mu.a", "mu.b", "mu.cp",
                                "sig.a", "sig.b", "sig.cp", "sig"))
