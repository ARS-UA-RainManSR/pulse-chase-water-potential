# Run model of PD only

library(tidyverse)
library(rjags)
load.module("dic")
library(mcmcplots)
library(broom.mixed)

dat_df <- read_csv("scripts/mod1 - pd/data_long.csv")

dat_df |> 
  group_by(date_col, house, period) |> 
  summarize(WP_m = mean(WP)) |> 
  group_by(date_col, period) |> 
  summarize(WP_sd = sd(WP_m))

datlist <- list(wp = dat_df$WP,
                D = as.vector(scale(dat_df$Dmean_period)),
                W1 = as.vector(scale(dat_df$SWC_1)),
                W2 = as.vector(scale(dat_df$SWC_2)),
                house = dat_df$house,
                N = nrow(dat_df),
                Nparam = 7,
                Nhouse = length(unique(dat_df$house)),
                Sa = 1)

# Function to generate initials
init <- function() {
  list(B = rnorm(datlist$Nparam, 0, 10),
       tau = runif(1, 0, 1),
       sig.eps.a = runif(1, 0, 1))
}

initslist <- list(init(), init(), init())

# Initialize model
jm <- jags.model("scripts/mod1 - wp/mod1-simple.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 10000)


# Monitor
params <- c("deviance", "Dsum",
            "B", "Bstar",
            "tau", "sig", 
            "sig.eps.a", "sig.eps", 
            "Estar"
)
coda.out <- coda.samples(jm,
                         variable.names = params,
                         n.iter = 3000,
                         n.thin = 15)

# Inspect chains visually
mcmcplot(coda.out, parms = c("deviance", "Dsum", 
                             "Bstar", "Estar",
                             "sig", "sig.eps"))

caterplot(coda.out, regex = "^Bstar\\[", reorder = FALSE)
caterplot(coda.out, parms = "Estar", reorder = FALSE)


# Replicated data

coda.rep <- coda.samples(jm, 
                         variable.names = c("wp.rep"),
                         n.iter = 3000,
                         n.thin = 15)


# Summarize replicated output
coda_sum <- tidyMCMC(coda.rep,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)


# Check model fit
pred <- cbind.data.frame(dat_df, coda_sum)

m1 <- lm(pred.mean ~ WP, data = pred)
summary(m1) # R2 = 0.6847

pred %>%
  ggplot(aes(x = WP, y = pred.mean, col = period)) +
  geom_abline(intercept = 0, slope = 1, col = "black", lty = 2) +
  geom_errorbar(aes(ymin = pred.lower, ymax = pred.upper),
                alpha = 0.25) +
  geom_point() +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  theme_bw(base_size = 12) +
  facet_wrap(~trt_s)
