# Changepoint model with parameters for S4 PD only
# Plot with SWP and VPD
# Derives the phases 0/1/2 for further analysis

library(coda)
library(tidyverse)
library(broom.mixed)
library(ggh4x)
library(RColorBrewer)


# Load data
load("scripts/mod4 - piecewise/s4_all.Rdata")
temp <- dat2 |> 
  filter(variable == "WP",
         period == "predawn") |> 
  mutate(plot = factor(ID))
# n=54 S4 predawns

vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "outside",
         period == "PD", 
         date >= min(temp$date_col),
         date <= max(temp$date_col)) |> 
  rename(Dmean = mean) |> 
  select(-period, -location, -sd) |> 
  mutate(days_since_pulse = 0:21)

swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         summer == "S4",
         depth == "0-12 cm",
         date >= min(temp$date_col),
         date <= max(temp$date_col)) |> 
  select(-period, -sd, depth) |> 
  rename(trt_s = summer, SWP_1 = mean) |> 
  mutate(days_since_pulse = 0:21)

# Load coda_params
load("scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")

param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Load coda_pred
load("scripts/mod4 - piecewise/coda/coda_pred_mod1.Rdata")

# Summarize replicated data
pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Join predicted with original and soil water potential for plotting
preds <- cbind.data.frame(temp, pred_sum) 

# Obtain other parameters of interest
cps <- param_sum |> 
  filter(grepl("^mu\\.cp", term))

maxy <- param_sum |> 
  filter(grepl("mu\\.maxy", term))

ab1 <- param_sum |> 
  filter(grepl("mu\\.a$", term) | grepl("mu\\.b\\[1", term)) |> 
  mutate(var = c("intercept", "slope"))

ab2 <- param_sum |> 
  filter(grepl("mu\\.a2", term) | grepl("mu\\.b\\[2", term)) |> 
  mutate(var = c("intercept", "slope"))

#  Establish colors
cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")
cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")
cols_div <- brewer.pal(7, "Accent")
display.brewer.pal(7, "Accent")

# Create labels 
labs <- c(lapply(c("PD", "0-12 cm"), function(i) bquote(Psi[.(i)])),
          lapply(c("PD"), function(i) bquote(VPD[.(i)])))


# Create dataframe for Phase labels
phases <- data.frame(lab = c("", "Phase 1", "Phase 2"),
                     xmin = c(-Inf, cps$pred.mean),
                     xmax = c(cps$pred.mean, Inf),
                     ymin = rep(0, 3),
                     ymax = rep(Inf, 3),
                     xmean = c(0, mean(cps$pred.mean), 
                               mean(c(cps$pred.mean[2], 21))),
                     ymean = rep(0.5, 3))
fig4 <- preds |> 
  ggplot() +
  geom_line(data = swp, 
            aes(x = days_since_pulse, y = SWP_1,
                color = "SWP")) +
  geom_line(data = vpd, 
            aes(x = days_since_pulse, y = Dmean-3,
                color = "VPD")) +
  geom_hline(data = maxy, 
             aes(yintercept = pred.mean),
             linetype = 2) +
  geom_rect(data = cps,
            aes(ymin = -Inf, ymax = Inf,
                xmin = pred.lower, xmax = pred.upper),
            color = "gray90", alpha = 0.15) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean)) +
  geom_abline(slope = ab1$pred.mean[2],
              intercept = ab1$pred.mean[1],
              linetype = 2) +
  geom_abline(slope = ab2$pred.mean[2],
              intercept = ab2$pred.mean[1],
              linetype = 2) +
  geom_point(aes(x = days_since_pulse, y = value,
                 color = "LWP"),
             size = 2) +
  geom_rect(data = phases,
            aes(xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax, 
                fill = lab),
            color = NA) +
  geom_text(data = phases,
            aes(x = xmean, y = ymean,
                label = lab),
            vjust = 1, hjust = 0.5) +
  scale_y_continuous(expression(paste(Psi, " (MPa)")), 
                     limits = c(-4.5, 0.5),
                     sec.axis = sec_axis(~.+3, 
                                         "VPD (kPa)",
                                         breaks = 0:3)) +
  scale_x_continuous(name = "Days since pulse", 
                     minor_breaks = seq(0, 21, 1),
                     breaks = seq(0, 21, 3),
                     limits = c(0, 21),
                     guide = "axis_minor") +
  scale_color_manual(values = c(cols_gn[4], cols_br_gn[1], "coral"),
                     labels = labs) +
  scale_fill_manual(values = cols_div[c(4,1,4)]) +
  theme_bw(base_size = 12) +
  theme(ggh4x.axis.ticks.length.minor = rel(1),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.25, 0.2),
        legend.background = element_blank(),
        axis.title.y.right = element_text(color = "coral"))+
  guides(fill = "none",
         color = guide_legend(override.aes = list(shape = c(16, NA, NA),
                                                  linetype = c(NA, 1, 1))))
ggsave(filename = "fig_scripts/fig4.png",
      plot = fig4,
      height = 3,
      width = 6,
      units = "in")

#### Calculations ####
# What range of SWP is encompassed by the shift from
# Phase 1 to Phase 2?

cp_range <- c(cps[2, 4], cps[2, 5])

swp |> 
  filter(days_since_pulse >= cp_range[1],
         days_since_pulse <= cp_range[2]) |> 
  pull(SWP_1) |> 
  range() # -1.22 to -0.65

swp |> 
  filter(days_since_pulse >= cp_range[1],
         days_since_pulse <= cp_range[2]) |> 
  pull(SWP_1) |> 
  mean() # -0.92

# As a rule of thumb, we define the threshold as SWP = -1 MPa
# Which in this soil is equivalent to VWC ~= 0.04 cm3/cm3


