# Pressure-volume model with TLP determined as changepoint
# between exponential decay + linear
# mod1 doesn't converge as written
# may need more constraint on parameter c (slope of linear) to be negative

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

# Quick plot
ggplot() +
  geom_point(data = wide |> 
               filter(period == "predawn"),
             aes(x = 1 - RWC, y = -1/WP,
                 color = ID))
  # scale_color_gradient(low = "coral", high = "royalblue") +
  # facet_wrap(~trt_s)

# Data list
dat_list <- list(y = -1/wide$WP,
                 irwc = 1-wide$RWC,
                 N = nrow(wide),
                 plot = factor(wide$ID),
                 Nplot = length(unique(wide$ID))
                 )
str(dat_list)  

# Inits list
inits <- function() {
  list(mu.a = rnorm(1, 0, 10),
       mu.b = rnorm(1, 0, 5),
       mu.c = rnorm(1, 0, 10),
       mu.cp = runif(1, 0.3, 0.5),
       tau = runif(1, 0, 1),
       sig.a = runif(1, 0, 1),
       sig.b = runif(1, 0, 1),
       sig.c = runif(1, 0, 1),
       sig.cp = runif(1, 0, 1))
}
inits_list <- list(inits(), inits(), inits())

# Compile model
jm <- jags.model("scripts/mod5 - pvcurve/mod1.JAGS",
                 data = dat_list,
                 inits = inits_list,
                 n.chains = 3)
update(jm, 10000)

# Params to monitor
params <- c("deviance", "Dsum", "R2",
            "a", "mu.a", "sig.a",
            "b", "mu.b", "sig.b",
            "c", "mu.c", "sig.c",
            "cp", "mu.cp", "sig.cp",
            "tau", "sig", "tlp", "mu.tlp"
            )
jm_coda <- coda.samples(jm, variable.names = params,
                        n.iter = 5000, thin = 5)

# Visualize chains
mcmcplot(jm_coda, parms = c("deviance", "Dsum", "R2", "sig",
                            "mu.a", "mu.b", "mu.c", "mu.cp",
                            "sig.a", "sig.b", "sig.c", "sig.cp",
                            "mu.tlp"))
mcmcplot(jm_coda, parms = c("a", "b", "c", "cp"))


a <- 3
b <- -4.5
c <- -1
cp <- 0.35

x <- seq(0, 1, 0.01)
y <- a*exp(b*x)
y2 <- c*x +(a*exp(b*cp) - c*cp)
plot(x, y)
points(x, y2, col = "red")
