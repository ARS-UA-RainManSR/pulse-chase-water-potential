# Develop model for S4 pulse only
# Rescale WP and RWC to 0-1 scale

library(tidyverse)
library(rjags)
load.module('dic')
library(mcmcplots)
# renv::install("fellmk/PostJAGS/postjags")
library(postjags)
library(broom.mixed)

# read in  cleaned datasets
wp_rwc <- read_csv("data_clean/wp_rwc.csv")|> 
  mutate(pulse_num2 = case_when(pulse_num %in% c(13, 7, 3) ~ 1,
                                pulse_num %in% c(15, 8) ~ 2))
# limit and rescale (combining 2 time periods)
df <- wp_rwc |> 
  filter(trt_s == "S4") |> 
  mutate(WP = (wp_mpa - min(wp_mpa))/(max(wp_mpa) - min(wp_mpa)),
         rwc = (RWC - min(RWC))/(max(RWC) - min(RWC))) |> 
  select(-wp_mpa, -RWC) |> 
  pivot_longer(cols = c(WP, rwc),
               names_to = "variable",
               values_to = "scaled_value") |> 
  mutate(period = factor(period, levels = c("predawn", "midday")),
         variable = case_when(variable == "rwc" ~ "rwc",
                              variable == "WP" ~ "wp") |> 
           factor(levels = c("wp", "rwc")),
         ID = factor(ID))

# check plot
df |> 
  ggplot(aes(x = days_since_pulse, y = scaled_value)) +
  geom_point(aes(color = period,
                 shape = ID)) +
  geom_line(aes(group = ID)) +
  scale_color_discrete(direction= -1) +
  facet_grid(cols = vars(variable),
             rows = vars(period),
             scales = "free_x",
             space = "free")

# data list
data_list <- list(vwc = df$scaled_value, # response variable, wp or rwc, scaled between 0 and 1
                 dop = df$days_since_pulse, # predictor, days since pulse, 0-21,
                 variable = df$variable, # 1 = wp, 2 = rwc
                 period = df$period, # 1 = predawn, 2 = midday
                 plot = df$ID, # six plots
                 N = nrow(df),
                 Nplot = length(unique(df$ID)))

# generate initials
init <- function() {
  list(mu.a = matrix(rnorm(4, 0, 10), ncol = 2),
       tau.a = matrix(runif(4, 0, 1), ncol = 2),
       mu.b = matrix(runif(4, 0, 1), ncol = 2),
       var.b = matrix(runif(4, 0, 1), ncol = 2),
       mu.c = matrix(rnorm(4, 0, 10), ncol = 2),
       tau.c = matrix(runif(4, 0, 1), ncol = 2),
       tau = runif(1, 0, 1))
}
inits_list <- list(init(), init(), init())

# Initiate model
jm <- jags.model("scripts/mod2 - ricker/ricker_S4_hier.JAGS",
                 inits = list(saved_state[[2]][[1]],
                              saved_state[[2]][[1]],
                              saved_state[[2]][[1]]),
                 data = data_list,
                 n.chains = 3)
update(jm, 10000)         
dic.samples(jm, 1000) # pD = 35

# parameters to monitor
params_vec <- c("a", "b", "c", "tau",
                "mu.a", "tau.a", "mu.b", "var.b", "mu.c", "tau.c",
                "peak.x", "peak.y", "sig",
                "deviance", "Dsum", "R2")
jm_coda <- coda.samples(jm, variable.names = params_vec,
                        n.iter = 10000,
                        thin = 10)

# View chains and mixing
mcmcplot(jm_coda, parms = c("Dsum", "R2", "deviance",
                            "mu.a", "mu.b", "mu.c", "tau",
                            "peak.x", "peak.y", "sig"))
caterplot(jm_coda, regex = "^a", reorder = FALSE)
caterplot(jm_coda, regex = "peak.x", reorder = FALSE)
caterplot(jm_coda, regex = "peak.y", reorder = FALSE)

# Convergence
gel <- gelman.diag(jm_coda, multivariate = FALSE)

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("Dsum", rowname) | grepl("R2", rowname) | grepl("deviance", rowname) )

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("sig", rowname) | grepl("tau", rowname) )

gel$psrf |> 
  data.frame() |> 
  tibble::rownames_to_column() |> 
  filter(grepl("^a", rowname) | grepl("^b", rowname) )

# Save state
final <- initfind(jm_coda, OpenBUGS = FALSE)
final[[1]]
saved_state <- removevars(final, variables = c(1:5, 9:11))
saved_state[[1]]

# temporary
ind <- which(colnames(jm_coda[[1]]) == "deviance")
mean(jm_coda[[1]][,ind])
mean(jm_coda[[2]][,ind])
mean(jm_coda[[3]][,ind])

# Summarize parameter values
param.sum <- tidyMCMC(jm_coda,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

params_df <- param.sum |> 
  filter(grepl("^a", term) | grepl("^b", term) | grepl("^c", term)) |>  
  tidyr::separate(term, 
                  into = c("term", "variable", "period")) |> 
  mutate(variable = case_when(variable == 1 ~ "wp",
                              variable == 2 ~ "rwc"),
         period = case_when(period == 1 ~ "predawn",
                            period == 2 ~ "midday")) 


peaks_long_df <- param.sum |> 
  filter(grepl("peak", term)) |> 
  tidyr::separate(term, 
                  into = c("temp1", "temp2", "variable", "period")) |> 
  mutate(variable = case_when(variable == 1 ~ "wp",
                              variable == 2 ~ "rwc"),
         period = case_when(period == 1 ~ "predawn",
                            period == 2 ~ "midday"),
         term = paste0(temp1, ".", temp2)) |> 
  select(-temp1, -temp2)

params_plot_df <- bind_rows(params_df, peaks_long_df) |> 
  mutate(period = factor(period, levels = c("predawn", "midday")),
         variable = factor(variable, levels = c("wp", "rwc")))

peaks_df <- param.sum |> 
  filter(grepl("peak", term)) |> 
  tidyr::separate(term, 
                  into = c("temp1", "temp2", "variable", "period")) |> 
  mutate(variable = case_when(variable == 1 ~ "wp",
                              variable == 2 ~ "rwc"),
         period = case_when(period == 1 ~ "predawn",
                            period == 2 ~ "midday")) |>
  select(-std.error) |> 
  pivot_wider(names_from = temp2,
              values_from = c(pred.mean, pred.lower, pred.upper)) |> 
  mutate(period = factor(period, levels = c("predawn", "midday")),
         variable = factor(variable, levels = c("wp", "rwc")))
  
# Run model for residuals
coda.resid <- coda.samples(jm,
                           variable.names = c("resid"),
                           n.iter = 3000)

resid.sum <- tidyMCMC(coda.resid,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

resids <- cbind.data.frame(df, resid.sum)

resids |> 
  ggplot() +
  geom_histogram(aes(x = pred.mean)) +
  facet_grid(cols = vars(variable),
             rows = vars(period),
             scales = "free_x",
             space = "free")

# Run model for replicated data
coda.pred <- coda.samples(jm,
                           variable.names = c("vwc.rep"),
                           n.iter = 3000)

pred.sum <- tidyMCMC(coda.pred,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(df, pred.sum)

ggplot() +
  geom_point(data = df, 
             aes(x = days_since_pulse, y = scaled_value,
                 color = period,
                 shape = factor(plot))) +
  geom_pointrange(data = preds, 
             aes(x = days_since_pulse, y = pred.mean,
                 ymin = pred.lower,
                 ymax = pred.upper),
             color = "gray80") +
  geom_rect(data = peaks_df,
            aes(xmin = pred.lower_x, xmax = pred.upper_x,
                ymin = pred.lower_y, ymax = pred.upper_y),
            alpha = 0.5) +
  geom_point(data = peaks_df,
             aes(x = pred.mean_x, 
                 y = pred.mean_y),
             color = "forestgreen") +
  scale_color_discrete(direction= -1) +
  facet_grid(cols = vars(variable),
             rows = vars(period),
             scales = "free_x",
             space = "free") +
  theme_bw(base_size = 14)

# Plot parameters
params_sig_df <- params_plot_df |> 
  select(-std.error) |> 
  pivot_wider(names_from = period,
              values_from = starts_with("pred")) |> 
  mutate(sig = ifelse((pred.mean_predawn <= pred.lower_midday |
                        pred.mean_predawn >=pred.upper_midday) &
                        (pred.mean_midday <= pred.lower_predawn |
                        pred.mean_midday >= pred.upper_predawn), TRUE, FALSE),
         param = "param",
         variable = factor(variable, levels = c("wp", "rwc"))) |> 
  filter(sig == TRUE) |> 
  rowwise() |> 
  mutate(yplace = mean(c_across(starts_with("pred.mean"))))

params_plot_df |> 
  mutate(param = "param") |> 
  ggplot(aes(x = param, y = pred.mean)) +
  geom_errorbar(aes(ymin = pred.lower,
                    ymax = pred.upper,
                    color = period),
                width = 0, alpha = 0.5,
                position = position_dodge(width = 0.9)) +
  geom_point(aes(color = period),
             size = 2.5,
             position = position_dodge(width = 0.9)) +
  geom_point(data = params_sig_df,
             aes(x = param, y = yplace),
             pch = 8) +
  scale_color_discrete(direction= -1) +
  facet_grid(cols = vars(variable),
             rows = vars(term),
             scales = "free_y",
             switch = "y") +
  theme_bw(base_size = 14) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        axis.title = element_blank(),
        axis.text.x = element_blank())
  
