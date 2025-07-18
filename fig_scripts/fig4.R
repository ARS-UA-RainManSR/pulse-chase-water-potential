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
         depth %in% c("0-10 cm", "25 cm"),
         date >= min(temp$date_col),
         date <= max(temp$date_col)) |> 
  select(-period, -sd, depth) |> 
  pivot_wider(names_from = depth,
              values_from = mean) |>
  rename(trt_s = summer, SWP_1 = `0-10 cm`, SWP_2 = `25 cm`) |> 
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

# Calculate start and end segments
x1 <- 0
y1 <- as.numeric(ab1$pred.mean[1] + x1*ab1$pred.mean[2])
x1; y1

x2 <- as.numeric(cps$pred.mean[1])
y2 <- as.numeric(maxy$pred.mean)
x2; y2

x3 <- as.numeric(cps$pred.mean[2])
y3 <- as.numeric(maxy$pred.mean)
x3; y3

x4 <- 21
y4 <- as.numeric(ab2$pred.mean[1] + x4*ab2$pred.mean[2])
x4; y4

# Make start and endpoints into dataframe
segs <- data.frame(label = c("initial", "phase 1", "phase2"),
                   x1 = c(x1, x2, x3),
                   y1 = c(y1, y2, y3),
                   x2 = c(x2, x3, x4),
                   y2 = c(y2, y3, y4))

# Calculate when SWP intersect cp2
cp2 <- as.numeric(cps$pred.mean[2])
floor(cp2);ceiling(cp2)
swp_sub <- swp |> 
  filter(days_since_pulse %in% 10:13)
m_swp <- lm(SWP_1 ~ days_since_pulse, data = swp_sub)
swp_pred <- cps |> 
  filter(term == "mu.cp[2]") |> 
  select(-term, -std.error) |> 
  pivot_longer(1:3) |> 
  rename(days_since_pulse = value) |> 
  mutate(pred_swp = predict(m_swp, to_pred)) |> 
  select(-days_since_pulse) |> 
  pivot_wider(names_from = name, 
              values_from = pred_swp)



#  Establish colors
cols_br_gn <- brewer.pal(7, "BrBG")
display.brewer.pal(7, "BrBG")
cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")
cols_div <- brewer.pal(7, "Spectral")
display.brewer.pal(7, "Spectral")

# Create labels 
labs <- c(lapply(c("PD", "0-10 cm"), function(i) bquote(Psi[.(i)])),
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

fig4 <-  preds |> 
  ggplot() +
  geom_line(data = swp, 
            aes(x = days_since_pulse, y = SWP_1,
                color = "SWP_1"),
            lty = "solid",
            size = 1) +
  # geom_line(data = swp,
  #           aes(x = days_since_pulse, y = SWP_2),
  #           lty = "solid") +
  geom_segment(data = segs, 
               aes(x = x1, y = y1,
                   xend = x2, yend = y2,
                   color = "LWP"),
               linetype = "dashed") +
  geom_rect(data = cps,
            aes(ymin = -Inf, ymax = Inf,
                xmin = pred.lower, xmax = pred.upper),
            fill = "gray90", alpha = 0.35,
            linewidth = 0) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             color = "gray50") +
  geom_rect(data = swp_pred,
            aes(ymin = pred.lower, ymax = pred.upper,
                xmin = -Inf, xmax = Inf),
            fill = "gray90", alpha = 0.35,
            linewidth = 0) +
  geom_hline(data = swp_pred,
             aes(yintercept = pred.mean),
             color = "gray50") +
  geom_point(aes(x = days_since_pulse, y = value,
                 color = "LWP"),
             size = 2.25) +
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
                     limits = c(-4.5, 0.5)) +
                     # sec.axis = sec_axis(~.+4.5, 
                     #                     "VPD (kPa)",
                     #                     breaks = 0:2)) +
  scale_x_continuous(name = "Days since P21 pulse", 
                     minor_breaks = seq(0, 21, 1),
                     breaks = seq(0, 21, 3),
                     limits = c(0, 21),
                     guide = "axis_minor") +
  scale_color_manual(values = c(cols_gn[4], cols_br_gn[1], "coral"),
                     labels = labs) +
  scale_fill_manual(values = cols_div[c(3,6,3)]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        # ggh4x.axis.ticks.length.minor = rel(1),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.25, 0.1),
        legend.background = element_blank()) +
        # axis.title.y.right = element_text(color = "coral")) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(shape = c(16, NA),
                                                  linetype = c(NA, 1))))

fig4

ggsave(filename = "fig_scripts/round2/fig4.png",
       plot = fig4,
       height = 3.5,
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

# As a rule of thumb, we define the threshold as SWP_1 = -1 MPa
# Which in this soil is equivalent to VWC ~= 0.04 cm3/cm3

swp |> 
  filter(days_since_pulse >= cp_range[1],
         days_since_pulse <= cp_range[2]) |> 
  pull(SWP_2) |> 
  range() # -1.22 to -0.65

swp |> 
  filter(days_since_pulse >= cp_range[1],
         days_since_pulse <= cp_range[2]) |> 
  pull(SWP_2) |> 
  mean() # -0.57

# As a rule of thumb, we define the threshold as SWP_2 = -0.6 MPa
# Which in this soil is equivalent to VWC ~= 0.0495 cm3/cm3


