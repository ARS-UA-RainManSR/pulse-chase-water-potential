# Control script for RWC model
# beta distribution with reparameterization

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
library(postjags)
library(broom.mixed)

# load data files
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
         phase = if_else(SWP_1 <= -1, "Phase 2", "Phase 1")) 

###### Set data ######

dat <- hyp_wide |>
  filter(phase == "Phase 2",
         !is.na(RWC_ind)) |> # 76 observations of RWC_ind and WP, but 61 observations of RWC
  select(-ID_long:-WPI2, -CNDI) |>
  mutate(logitRWC = log(RWC/(1-RWC)))

# plot raw
dat |>
  ggplot(aes(x = RWC, y = WP, color = period2)) +
  geom_point()

# plot Gardner
dat |> 
  ggplot(aes(x = log(RWC), y = log(abs(WP)))) +
  geom_point(aes(col = period2)) +
  stat_smooth(aes(group = period2),
              method = "lm") # hierarchical by period seems warranted

# plot missing data model
dat |>
  ggplot(aes(x = RWC_ind, y = RWC, color = period2)) +
  geom_point() +
  stat_smooth(aes(group = period2),
              method = "lm") # no need to be hierarchical by period, lots of error

summary(lm(log(abs(WP)) ~ log(RWC), data = dat))
# summary(lm(logitRWC ~ scale(RWC_ind), data = dat))
# summary(lm(logitRWC ~ scale(RWC_ind)*period2, data = dat))

dat_list <- list(log.abs.WP = log(abs(dat$WP)),
                 RWC = dat$RWC,
                 N = nrow(dat),
                 period = dat$period2,
                 # center and standardize predictor variable for RWC
                 RWC_ind = as.vector(scale(dat$RWC_ind)))
str(dat_list)

inits <- function() {
  list(inv.b = rnorm(2, 0, 1),
       log.a = rnorm(2, 0, 1),
       B = rnorm(2, 0, 10),
       tau = runif(1, 0, 1),
       tau.rwc = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

load("scripts/mod6 - rwc/inits/inits.Rdata")

# Jags model
jm <- jags.model(file = "scripts/mod6 - rwc/PV-hyperspec.JAGS",
                 data = dat_list,
                 # inits = inits_list,
                 inits = saved_state[[2]],
                 n.chains = 3)

# update(jm, 100000)

# Sample posterior
vars <- c("Dsum", "deviance", "R2",
          "inv.b", "log.a", "b.mu", "a.mu",
          "B", "tau", "sig", "tau.rwc", "sig.rwc")
coda_params <- coda.samples(jm, variable.names = vars,
                            n.iter = 10000, thin = 10)

# View posterior
mcmcplot(coda_params, parms = c("Dsum", "deviance", "R2",
                                "inv.b", "b.mu", 
                                "log.a", "a.mu",
                                "B", "sig", "sig.rwc"))
caterplot(coda_params, parms = "inv.b")
caterplot(coda_params, parms = "log.a")
caterplot(coda_params, parms = "a.mu")
caterplot(coda_params, parms = "b.mu")
caterplot(coda_params, parms = "B")
caterplot(coda_params, parms = "R2")

# Check for convergence
gel <- gelman.diag(coda_params, multivariate = FALSE)

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("Dsum", rowname) | grepl("R2", rowname) | grepl("deviance", rowname) )

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("inv.b", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("log.a", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^B", rowname))

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("tau", rowname) | grepl("sig", rowname))

# Save state
# final <- initfind(coda_params, OpenBUGS = FALSE)
# final[[1]]
# saved_state <- removevars(final, variables = c(2:5, 8:9))
# saved_state[[1]]
# save(saved_state, file = "scripts/mod6 - rwc/inits/inits.Rdata")

save(coda_params, file = "scripts/mod6 - rwc/coda/coda_params.Rdata")


# Sample predicted WP
coda_rwc <- coda.samples(jm,
                          variable.names = c("RWC"),
                          n.iter = 10000, thin = 10)

save(coda_rwc, file = "scripts/mod6 - rwc/coda/coda_pred_rwc.Rdata")


# Sample predicted WP
coda_pred <- coda.samples(jm,
                          variable.names = c("log.abs.WP.rep"),
                          n.iter = 10000, thin = 10)

save(coda_pred, file = "scripts/mod6 - rwc/coda/coda_pred_wp.Rdata")


pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(dat, pred_sum) |>
  mutate(missing = if_else(is.na(RWC), "Imputed RWC", "Observed RWC") |>
           factor(levels = c("Observed RWC", "Imputed RWC")))

table(preds$missing) # 15 missing RWC's
# Linear fit
m1 <- lm(-1*exp(pred.mean) ~ WP, data = filter(preds, missing == "Observed RWC"))
s1 <- summary(m1)
s1 # R2 = 0.7729

m2 <- lm(-1*exp(pred.mean) ~ WP, data = filter(preds, missing == "Imputed RWC"))
s2 <- summary(m2)
s2 # R2 = 0.8929

# Make df of model parameters
fit_df <- data.frame(missing = c("Observed RWC", "Imputed RWC"),
                     slope = c(s1$coefficients[2,1], s2$coefficients[2,1]),
                     int = c(s1$coefficients[1,1], s2$coefficients[1,1]),
                     adj.R2 = c(s1$adj.r.squared, s2$adj.r.squared)) |>
  mutate(lab1 = paste0("R^2==", round(adj.R2, 3)),
         lab2 = paste0("Bias==", round(slope, 3)),
         missing = factor(missing, levels = c("Observed RWC", "Imputed RWC")))


# fit
preds |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0,) +
  geom_abline(data = fit_df,
              aes(slope = slope, intercept = int),
              lty = "dashed") +
  geom_errorbar(aes(x = WP,
                    ymin = -1*exp(pred.lower),
                    ymax = -1*exp(pred.upper),
                    color = period2),
                alpha = 0.25) +
  geom_point(aes(x = WP, y = -1*exp(pred.mean), color = period2)) +
  geom_text(data = fit_df,
            aes(x = -.5, y = -8, label = lab1),
            parse = TRUE,
            hjust = 1) +
  geom_text(data = fit_df,
            aes(x = -.5, y = -8.75, label = lab2),
            parse = TRUE,
            hjust = 1) +
  scale_x_continuous(expression(paste("Observed ", Psi, " (MPa)")),
                     limits = c(-9.1, 0)) +
  scale_y_continuous(expression(paste("Predicted ", Psi, " (MPa)")),
                     limits = c(-9.1, 0)) +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~missing)


