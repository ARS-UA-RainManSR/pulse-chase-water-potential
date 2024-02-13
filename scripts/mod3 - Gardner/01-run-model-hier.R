# Control script for Gardner model

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(broom.mixed)

# Read in data
mrc <- read_csv("data_clean/moisture_release.csv") |> 
  rename(wp = mpa)

# Check mean of vwc
vwc <- read_csv("data_clean/vwc_daily_daytime.csv")
vwc |> 
  filter(period == "morn") |> 
  group_by(summer, depth) |> 
  summarize(vwc_mean = mean(mean, na.rm = TRUE)) |> 
  ggplot() +
  geom_histogram(aes(x = vwc_mean))

mean(mrc$vwc) # .0419
log(mean(mrc$vwc)) # -3.17
log(0.05) # -2.995

# Use log(0.05) to center data, no need to standardize as goal is prediction

# Plot raw
mrc |> 
  ggplot(aes(x = vwc, y = wp)) +
  geom_point(aes(col = as.factor(depth)))

# Plot for Gardner model
mrc |> 
  ggplot(aes(x = log(vwc), y = log(abs(wp)))) +
  geom_point(aes(col = as.factor(depth))) +
  facet_wrap(~house)

mean(log(mrc$vwc))

# Data list
data_list <- list(log.y = log(abs(mrc$wp)),
                  log.x = log(mrc$vwc) - log(0.05),
                  N = nrow(mrc),
                  depth = factor(mrc$depth),
                  house = factor(mrc$house))

# Initials
inits <- function() {
  list(inv.b.mu = rnorm(2, 0, 10),
       log.a.mu = rnorm(2, 0, 10),
       sig.inv.b = runif(1, 0, 1),
       sig.log.a = runif(1, 0, 1),
       tau = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

load("scripts/mod3 - Gardner/inits/inits_hier.Rdata")

# Compile model
jm <- jags.model(file = "scripts/mod3 - Gardner/Gardner_hier.JAGS",
                 inits = saved_state[[2]],
                 data = data_list,
                 n.chains = 3)
# update(jm, 10000)
dic.samples(jm, 1000)

# Sample posterior
params <- c("deviance", "Dsum", "R2",
            "inv.b.mu", "log.a.mu", "inv.b", "log.a",
            "sig", "tau", "sig.inv.b", "sig.log.a",
            "b.mu", "a.mu")
coda_params <- coda.samples(jm, variable.names = params,
                            n.iter = 10000,
                            thin = 10)

# View posterior
mcmcplot(coda_params, parms = c("deviance", "Dsum", "R2",
                                "b.mu", "a.mu", "inv.b.mu", "log.a.mu",
                                "sig", "sig.inv.b", "sig.log.a"))
mcmcplot(coda_params, parms = c("log.a", "inv.b"))
caterplot(coda_params, parms = "inv.b.mu", reorder = FALSE)
caterplot(coda_params, parms = "log.a.mu", reorder = FALSE)
caterplot(coda_params, parms = "b.mu", reorder = FALSE)
caterplot(coda_params, parms = "a.mu", reorder = FALSE)
caterplot(coda_params, parms = "inv.b", reorder = FALSE)
caterplot(coda_params, parms = "log.a", reorder = FALSE)

# Check for convergence
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
  filter(grepl("^inv.b", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^log.a", rowname))


# Save state
# final <- initfind(coda_params, OpenBUGS = FALSE)
# final[[1]]
# saved_state <- removevars(final, variables = c(1:5, 7, 9))
# saved_state[[1]]
# save(saved_state, file = "scripts/mod3 - Gardner/inits/inits_hier.Rdata")

save(coda_params, file = "scripts/mod3 - Gardner/coda/coda_params_hier.Rdata")

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

resids <- cbind.data.frame(mrc, resid_sum)

resids |> 
  ggplot() +
  geom_histogram(aes(x = pred.mean)) +
  facet_grid(cols = vars(depth),
             rows = vars(house),
             scales = "free_x",
             space = "free")

save(coda_resid, file = "scripts/mod3 - Gardner/coda/coda_resid_hier.Rdata")

# Sample predicted values
coda_pred <- coda.samples(jm,
                          variable.names = c("log.y.rep"),
                          n.iter = 3000)

pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(mrc, pred_sum)

# fit
preds |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  geom_point(aes(x = log(abs(wp)), y= pred.mean)) +
  facet_wrap(~depth)

save(coda_pred, file = "scripts/mod3 - Gardner/coda/coda_pred_hier.Rdata")

# On original scales
preds |> 
  ggplot() +
  # geom_hline(yintercept = 2) +
  geom_point(aes(x = vwc, y = abs(wp), color = factor(depth))) +
  geom_errorbar(aes(x = vwc, 
                    ymin = exp(pred.lower),
                    ymax = exp(pred.upper)),
                alpha = 0.25) +
  geom_point(aes(x = vwc, y = exp(pred.mean))) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)"))) +
  facet_wrap(~depth) +
  theme_bw(base_size = 14) +
  guides(color = "none")


# Create predictions from site-level params
param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

site_p <- param_sum |> 
  filter(grepl("inv.b.mu", term) | grepl("log.a.mu", term))

x <- seq(log(min(vwc$mean, na.rm = TRUE)) - log(0.05),
         log(max(vwc$mean, na.rm = TRUE)) - log(0.05),
         by = 0.01)
invb_1 <- site_p$pred.mean[1]
invb_2 <- site_p$pred.mean[2]
loga_1 <- site_p$pred.mean[3]
loga_2 <- site_p$pred.mean[4]

y_1 <- -1*invb_1*(x - loga_1)
y_2 <- -1*invb_2*(x - loga_2)

pred_m <- data.frame(x = x,
                     y1 = y_1,
                     y2 = y_2) |> 
  mutate(swp1 = -1*exp(y1),
         swp2 = -1*exp(y2), 
         vwc = exp(x + log(0.05))) |> 
  select(-y1, -y2) |> 
  pivot_longer(cols = 2:3,
               names_to = "depth",
               values_to = "swp") |> 
  mutate(depth = case_when(depth == "swp1" ~ "10",
                           depth == "swp2" ~ "25"))

ggplot() +
  geom_line(data = pred_m, 
            aes(x = vwc,
                y = swp,
                color = depth)) 

mrc |> 
  ggplot() +
  geom_point(aes(x = vwc, y = abs(wp), color = factor(depth)),
             alpha = 0.5) +
  geom_line(data = pred_m, 
            aes(x = vwc,
                y = abs(swp),
                color = depth)) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")),
                     limits = range(vwc$mean, na.rm = TRUE)) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)")),
                     limits = c(0, 10)) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")
