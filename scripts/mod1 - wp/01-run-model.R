# Run model of PD only

library(tidyverse)
library(rjags)
load.module("dic")
library(mcmcplots)
library(broom.mixed)

load(file = "scripts/mod1 - wp/wp_all.Rdata")

str(wp_all)

datlist <- list(wp = wp_all$value,
                D = as.vector(scale(wp_all$Dmean)), # matched to predawn or midday
                SWP1 = as.vector(scale(wp_all$SWP_1)), # morning only 4:30 am to 12 noon
                ID = factor(wp_all$ID),
                N = nrow(wp_all),
                Nparam = 4,
                Nid = length(unique(wp_all$ID)),
                Sa = 1)
str(datlist)

# Function to generate initials
init <- function() {
  list(B = rnorm(datlist$Nparam, 0, 10),
       tau = runif(1, 0, 1),
       sig.eps.a = runif(1, 0, 1))
}

initslist <- list(init(), init(), init())

str(initslist)
# Initialize model
jm <- jags.model("scripts/mod1 - wp/mod1-simple.JAGS",
                 data = datlist,
                 inits = initslist,
                 n.chains = 3)

update(jm, n.iter = 20000)

dic.samples(jm, n.iter = 10000)


# Monitor
params <- c("deviance", "Dsum", "R2",
            "B", "Bstar",
            "tau", "sig", 
            "sig.eps.a", "sig.eps", 
            "Estar"
)
coda.out <- coda.samples(jm,
                         variable.names = params,
                         n.iter = 15000,
                         n.thin = 15)

# Inspect chains visually
mcmcplot(coda.out, parms = c("deviance", "Dsum", "R2",
                             "Bstar", "Estar",
                             "sig", "sig.eps"))

caterplot(coda.out, regex = "^Bstar\\[", reorder = FALSE)
caterplot(coda.out, parms = "Estar", reorder = FALSE)


# Replicated data

coda.rep <- coda.samples(jm, 
                         variable.names = c("wp.rep"),
                         n.iter = 15000,
                         n.thin = 15)


# Summarize replicated output
rep_sum <- tidyMCMC(coda.rep,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)


# Check model fit
pred <- cbind.data.frame(wp_all, rep_sum)

m1 <- lm(pred.mean ~ value, data = pred)
summary(m1) # R2 = 0.7832

pred %>%
  ggplot(aes(x = value, y = pred.mean, col = period)) +
  geom_abline(intercept = 0, slope = 1, col = "black", lty = 2) +
  geom_errorbar(aes(ymin = pred.lower, ymax = pred.upper),
                alpha = 0.25) +
  geom_point() +
  scale_x_continuous("Observed") +
  scale_y_continuous("Predicted") +
  theme_bw(base_size = 12)
