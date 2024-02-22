# hierarchical model for 2 changepoints
# reperameterized for two slopes and one intercept
# single variable - predawn LWP

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)
library(broom.mixed)

load("scripts/mod4 - piecewise/s4_all.Rdata")


# plot

dat2 |>
  ggplot(aes(x = days_since_pulse, y = value)) +
  geom_point(aes(color = ID)) +
  geom_line(aes(color = ID)) +
  facet_grid(rows = vars(variable),
             cols = vars(period),
             scales = "free_y")

# data list for predawns only
temp <- dat2 |> 
  filter(variable == "WP",
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
       # mu.maxy = rnorm(1, 0, 10),
       mu.cp = c(runif(1, 0.5, 5), runif(1, 10, 15)),
       tau = runif(1, 0, 1),
       sig.a = runif(1, 0, 10),
       sig.b = runif(2, 0, 10),
       sig.cp = runif(2, 0, 10))
       # sig.maxy = runif(1, 0, 10))
}
inits_list <- list(inits(), inits(), inits())
load("scripts/mod4 - piecewise/inits/inits_mod1.Rdata")

# Compile model
jm <- jags.model(file = "scripts/mod4 - piecewise/mod1.JAGS",
                 data = data_list,
                 inits = saved_state[[2]],
                 n.chains = 3)
update(jm, 10000)
dic.samples(jm, 10000)

# Sample 
params_vec <- c("deviance", "Dsum", "R2",
                "mu.a", "a", "mu.b", "b", "mu.cp", "cp", 
                "maxy", "mu.maxy", "a2", "mu.a2",
                "sig.a", "sig.b", "sig.cp", "sig", "tau")
coda_params <- coda.samples(jm, variable.names = params_vec,
                            n.iter = 10000,
                            thin = 10)

# View posterior chains
# mcmcplot(coda_params, parms = c("a", "b", "cp"))
mcmcplot(coda_params, parms = c("deviance", "Dsum", "R2",
                                "mu.a", "mu.b", "mu.cp", "maxy", "mu.maxy",
                                "sig.a", "sig.b", "sig.cp", "sig"))
save(coda_params, file = "scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")
# Restart values
# newinits <- initfind(coda_params, OpenBUGS = FALSE)
# newinits[[1]]
# saved_state <- removevars(newinits, variables = c(1:6, 10:11))
# saved_state[[1]]
# save(saved_state, file = "scripts/mod4 - piecewise/inits/inits_mod1.Rdata")

# check convergence
gel <- gelman.diag(coda_params, multivariate = FALSE)

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("Dsum", rowname) | grepl("R2", rowname) | grepl("deviance", rowname) )

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^sig", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^mu", rowname))

# Sample residuals
coda_resid <- coda.samples(jm,
                           variable.names = c("resid"),
                           n.iter = 10000,
                           thin = 10)

resid_sum <- tidyMCMC(coda_resid,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

resids <- cbind.data.frame(temp, resid_sum)

resids |> 
  ggplot() +
  geom_histogram(aes(x = pred.mean))

save(coda_resid, file = "scripts/mod4 - piecewise/coda/coda_resid_mod1.Rdata")


# Sample predicted values
coda_pred <- coda.samples(jm,
                          variable.names = c("y.rep"),
                          n.iter = 10000,
                          thin = 10)

pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(temp, pred_sum)

preds |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_errorbar(aes(x = value, 
                    ymin = pred.lower,
                    ymax = pred.upper)) +
  geom_point(aes(x = value, y= pred.mean))

summary(lm(pred.mean~value, data = preds)) # R2 = 0.9272

save(coda_pred, file = "scripts/mod4 - piecewise/coda/coda_pred_mod1.Rdata")
