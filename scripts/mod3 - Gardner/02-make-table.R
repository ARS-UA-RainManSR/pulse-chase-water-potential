# Invert Gardner model
# Run through with range of SWC and posterior parameter sets
# for both _hier and _site versions, for a total of 3 sets of predictions
# site, 10, 25 
library(tidyverse)
library(coda)
library(udunits2)
library(broom.mixed)

# Define inverse function
# Input SWC is [theta.r, theta.s]
# Oputputu is SWP in MPa
VWC_to_SWP <- function(vwc, invb, loga) {
  if(sum(vwc < 1 & vwc > 0) == length(vwc)) {
    # Convert vwc to log and centered scale
    y = -1*invb*(log(vwc) - log(0.05) - loga)
    # Return soil water potential in MPa
    return(-1 * exp(y))
  } else {
    print("Make sure VWC is in units of cm^3 cm^-3")
  }
}

VWC_to_SWP_vec <- Vectorize(FUN = VWC_to_SWP, ,
                            vectorize.args = c("invb", "loga"),
                            SIMPLIFY = TRUE)

# Load parameters
load("scripts/mod3 - Gardner/coda/coda_params_site.Rdata")
params_site <- coda_params
load("scripts/mod3 - Gardner/coda/coda_params_hier.Rdata")
params_hier <- coda_params

# Calculate posterior means
site_df <- tidyMCMC(params_site, 
                      conf.int =  TRUE, 
                      conf.method = "HPDinterval",
                      conf.level = 0.95) |> 
  filter(grepl("inv.b.mu", term) | grepl("log.a.mu", term)) |> 
  mutate(depth = "site")

hier_df <- tidyMCMC(params_hier, 
                    conf.int =  TRUE, 
                    conf.method = "HPDinterval",
                    conf.level = 0.95) |> 
  filter(grepl("inv.b.mu", term) | grepl("log.a.mu", term)) |> 
  mutate(term_original = term) |> 
  tidyr::separate(term, into = c("term", "term2", "term3", "depth")) |> 
  mutate(term = paste0(term, ".", term2, ".", term3)) |> 
  select(-term2, -term3)

# Test single curve with posterior means
test <- data.frame(vwc_in = seq(0.02, 0.12, .0001))
test$swp_out_site <- VWC_to_SWP(test$vwc_in, site_df$estimate[1], site_df$estimate[2])
foo <- VWC_to_SWP_vec(test$vwc_in, hier_df$estimate[hier_df$term == "inv.b.mu"], hier_df$estimate[hier_df$term == "log.a.mu"])
test2 <- cbind.data.frame(test, foo)
colnames(test2)[3:4] <- c("swp_out_1", "swp_out_2")

test2 |> 
  ggplot(aes(x = vwc_in)) +
  geom_point(aes(y = swp_out_site,
                 color = "site")) +
  geom_point(aes(y = swp_out_1,
                 color = "1")) +
  geom_point(aes(y = swp_out_2,
                 color = "2")) +
  scale_x_continuous(expression(paste(Theta, " (", cm^3, " ", cm^-3, ")"))) +
  scale_y_continuous(expression(paste(Psi[soil], " (-MPa)"))) +
  theme_bw(base_size = 12)

# Apply for each of 3000
# Assemble site-level and depth-level parameters
coda_site <- rbind.data.frame(params_site[[1]], params_site[[2]], params_site[[3]]) |> 
  select(starts_with("inv.b.mu"), starts_with("log.a.mu"))

coda_1 <- rbind.data.frame(params_hier[[1]], params_hier[[2]], params_hier[[3]]) %>%
  select(filter(hier_df, depth == "1")$term_original)

coda_2 <- rbind.data.frame(params_hier[[1]], params_hier[[2]], params_hier[[3]]) %>%
  select(filter(hier_df, depth == "2")$term_original)

# Custom functions
vwc_apply <- function(vec, vwc) { # vector of parameters c(inv.b, log.a)
    # Convert vwc to log and centered scale
  y = -1*vec[1]*(log(vwc) - log(0.05) - vec[2])
  # Return soil water potential in MPa
  return(-1 * exp(y))
}
cnt <- function(x) {sum(!is.na(x))}

# Calculate swp for all sets of parameters

out_site <- apply(coda_site, MARGIN = 1, FUN = vwc_apply,
                 vwc = seq(0.015, 0.45, .0001))

out_1 <- apply(coda_1, MARGIN = 1, FUN = vwc_apply,
               vwc = seq(0.015, 0.45, .0001))

out_2 <- apply(coda_2, MARGIN = 1, FUN = vwc_apply,
               vwc = seq(0.015, 0.45, .0001))

# Summarize to number, median, and central 50th percentile
out_df <- cbind.data.frame(vwc  = seq(0.015, 0.45, .0001),
                           n = apply(out_site, 1, FUN = cnt),
                           SWP_MPa_50 = apply(out_site, 1, FUN = median, na.rm = TRUE),
                           SWP_MPa_25 = apply(out_site, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           SWP_MPa_75 = apply(out_site, 1, FUN = quantile, probs = 0.75, na.rm = TRUE),
                           `1_SWP_MPa_50` = apply(out_1, 1, FUN = median, na.rm = TRUE),
                           `1_SWP_MPa_25` = apply(out_1, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           `1_SWP_MPa_75` = apply(out_1, 1, FUN = quantile, probs = 0.75, na.rm = TRUE),
                           `2_SWP_MPa_50` = apply(out_2, 1, FUN = median, na.rm = TRUE),
                           `2_SWP_MPa_25` = apply(out_2, 1, FUN = quantile, probs = 0.25, na.rm = TRUE),
                           `2_SWP_MPa_75` = apply(out_2, 1, FUN = quantile, probs = 0.75, na.rm = TRUE)) %>%
  filter(vwc < 0.45)

# Plot with error for sanity check
out_df |> 
  filter(vwc < 0.05) %>%
  ggplot() +
  geom_errorbar(aes(x = vwc,
                    ymin = SWP_MPa_25,
                    ymax = SWP_MPa_75),
                width = 0,
                alpha = 0.15,
                color = "forestgreen") +
  geom_point(aes(x = vwc,
                 y = SWP_MPa_50),
             color = "forestgreen") +
  theme_bw(base_size = 14)

#### Create lookup table based on predictions all predictions
# Include the median and central 50th percentile

lookup <- out_df %>%
  filter(n > 1000) # all vwc's have values

# Save
save(lookup, file = "source/lookup.Rdata")

