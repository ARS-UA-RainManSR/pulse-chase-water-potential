# Stack of S4 pulse responses across variables
# Note where the changespoints are across all plots

library(coda)
library(broom.mixed)
library(tidyverse)
library(RColorBrewer)
library(ggh4x)
library(cowplot)

# Need LWP, RWC, NDVI, PRI, GPP, and GCC
# as well as location of changepoints
# no env data needed

# Load coda_params
load("scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")

# Summarize
param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") |>
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Obtain posterior summary of changepoints
cps <- param_sum |> 
  filter(grepl("^mu\\.cp", term))
cp1 <- cps[1,]
cp2 <- cps[2,]

# Load leaf phys data
wp <- read_csv("data_clean/wp_rwc_long.csv") |>
  filter(variable == "WP",
         trt_s == "S4") |>
  mutate(period = case_when(period == "predawn" ~ "PD",
                            period == "midday" ~ "MD") |>
           factor(levels = c("PD", "MD")), 
         days_since_pulse = if_else(date_col == as.Date("2023-09-04"),
                                    21, days_since_pulse))

wp_sum <- wp |>
  group_by(days_since_pulse, period) |>
  summarize(WP_m = mean(value),
            WP_sd = sd(value))

rwc <- read_csv("data_clean/wp_rwc_long.csv") |>
  filter(variable == "RWC",
         trt_s == "S4") |>
  mutate(period = case_when(period == "predawn" ~ "PD",
                            period == "midday" ~ "MD") |>
           factor(levels = c("PD", "MD")),
         days_since_pulse = if_else(date_col == as.Date("2023-09-04"),
                                    21, days_since_pulse))



rwc_sum <- rwc |>
  group_by(days_since_pulse, period) |>
  summarize(RWC_m = mean(value),
            RWC_sd = sd(value))


# Load spectral indices
# days since pulse
dsp <- wp |>
  select(date_col, days_since_pulse) |>
  distinct()

ndvi <- read_csv("data_clean/hyp_indices.csv") |> 
  filter(trt_s == "S4") |>
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix")) |>
  select(1:8, NDVI, ID) |>
  left_join(dsp) |>
  mutate(period = case_when(period == "predawn" ~ "PD",
                            period == "midday" ~ "MD") |>
           factor(levels = c("PD", "MD")))

ndvi_sum <- ndvi |>
  group_by(days_since_pulse, period) |>
  summarize(ndvi_m = mean(NDVI),
            ndvi_sd = sd(NDVI))

pri <- read_csv("data_clean/hyp_indices.csv") |> 
  filter(trt_s == "S4") |>
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix")) |>
  select(1:8, PRI, ID) |>
  left_join(dsp) |>
  mutate(period = case_when(period == "predawn" ~ "PD",
                            period == "midday" ~ "MD") |>
           factor(levels = c("PD", "MD")))



pri_sum <- pri |>
  group_by(days_since_pulse, period) |>
  summarize(pri_m = mean(PRI),
            pri_sd = sd(PRI))

# Load GPP
gpp <- read_csv("data/plotgas2023.csv") |>
  mutate(date_col = lubridate::mdy(Date,
                                   tz = "America/Phoenix"),
         ER2 = ifelse(ER < 0, 0, ER), # Restrict ER to positive values
         GPP = -NEE + ER2) |> 
  filter(PT == "S4",
         date_col >= min(wp$date_col),
         date_col <= max(wp$date_col)) |>
  left_join(dsp)

gpp_sum <- gpp |>
  group_by(days_since_pulse) |>
  summarize(gpp_m = mean(GPP),
            gpp_sd = sd(GPP),
            et_m = mean(ET),
            et_sd = sd(ET))


gpp_sum |>
  ggplot(aes(x = days_since_pulse)) +
  geom_point(data = gpp,
             aes(y = GPP),
             alpha = 0.25) +
  geom_errorbar(aes(x = days_since_pulse,
                    ymin = gpp_m - gpp_sd,
                    ymax = gpp_m + gpp_sd),
                width = 0) +
  geom_point(aes(x = days_since_pulse, y = gpp_m)) +
  geom_line(aes(x = days_since_pulse, y = gpp_m)) +
  theme_bw()

# Assemble panels
cols_gn <- brewer.pal(4, "Paired")

##### WP #####
fig6a <- wp |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = value,
                 color = period),
             alpha = 0.25) +
  geom_errorbar(data = wp_sum,
                aes(x = days_since_pulse, 
                    ymin = WP_m - WP_sd,
                    ymax = WP_m + WP_sd,
                    color = period),
                width = 0) +
  geom_point(data = wp_sum,
             aes(x = days_since_pulse,
                 y = WP_m,
                 color = period),
             size = 2) +
  geom_line(data = wp_sum,
            aes(x = days_since_pulse, 
                y = WP_m,
                color = period)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_color_manual(values = cols_gn[c(4,3)]) +
  guides(color = guide_legend(override.aes = list(linetype = 0))) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.3, 0.2),
        legend.background = element_blank())

##### RWC #####
fig6b <- rwc |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = value,
                 color = period),
             alpha = 0.25) +
  geom_errorbar(data = rwc_sum,
                aes(x = days_since_pulse, 
                    ymin = RWC_m - RWC_sd,
                    ymax = RWC_m + RWC_sd,
                    color = period),
                width = 0) +
  geom_point(data = rwc_sum,
             aes(x = days_since_pulse,
                 y = RWC_m,
                 color = period),
             size = 2) +
  geom_line(data = rwc_sum,
            aes(x = days_since_pulse, 
                y = RWC_m,
                color = period)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous(expression(paste("RWC (g ", g^-1, ")"))) +
  scale_color_manual(values = cols_gn[c(4,3)]) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

##### NDVI #####
fig6c <- ndvi |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = NDVI,
                 color = period),
             alpha = 0.25) +
  geom_errorbar(data = ndvi_sum,
                aes(x = days_since_pulse, 
                    ymin = ndvi_m - ndvi_sd,
                    ymax = ndvi_m + ndvi_sd,
                    color = period),
                width = 0) +
  geom_point(data = ndvi_sum,
             aes(x = days_since_pulse,
                 y = ndvi_m,
                 color = period),
             size = 2) +
  geom_line(data = ndvi_sum,
            aes(x = days_since_pulse, 
                y = ndvi_m,
                color = period)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous("NDVI") +
  scale_color_manual(values = cols_gn[c(4,3)]) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

##### PRI #####
fig6d <- pri |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = PRI,
                 color = period),
             alpha = 0.25) +
  geom_errorbar(data = pri_sum,
                aes(x = days_since_pulse, 
                    ymin = pri_m - pri_sd,
                    ymax = pri_m + pri_sd,
                    color = period),
                width = 0) +
  geom_point(data = pri_sum,
             aes(x = days_since_pulse,
                 y = pri_m,
                 color = period),
             size = 2) +
  geom_line(data = pri_sum,
            aes(x = days_since_pulse, 
                y = pri_m,
                color = period)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous("PRI") +
  scale_color_manual(values = cols_gn[c(4,3)]) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

##### GPP #####
fig6e <- gpp |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = GPP),
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum,
                aes(x = days_since_pulse, 
                    ymin = gpp_m - gpp_sd,
                    ymax = gpp_m + gpp_sd),
                width = 0) +
  geom_point(data = gpp_sum,
             aes(x = days_since_pulse,
                 y = gpp_m),
             size = 2) +
  geom_line(data = gpp_sum,
            aes(x = days_since_pulse, 
                y = gpp_m)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous(expression(paste("GPP (", mu, "mol ", CO[2], " ", m^-2, s^-1, ")"))) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

## 6f - ET? ##
fig6f <- gpp |>
  ggplot() +
  geom_rect(data = cps,
            aes(xmin = pred.lower, xmax = pred.upper,
                ymin = -Inf, ymax = Inf),
            color = "gray90", alpha = 0.15) +
  geom_point(aes(x = days_since_pulse,
                 y = ET),
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum,
                aes(x = days_since_pulse, 
                    ymin = et_m - et_sd,
                    ymax = et_m + et_sd),
                width = 0) +
  geom_point(data = gpp_sum,
             aes(x = days_since_pulse,
                 y = et_m),
             size = 2) +
  geom_line(data = gpp_sum,
            aes(x = days_since_pulse, 
                y = et_m)) +
  geom_vline(data = cps,
             aes(xintercept = pred.mean),
             lty = "longdash") +
  scale_x_continuous("Days since pulse") +
  scale_y_continuous(expression(paste("ET (mmol ", H[2], O, " ", m^-2, s^-1, ")"))) +
  guides(color = "none") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank())

##### combine #####

fig6 <- plot_grid(fig6a, fig6b, fig6c, fig6d, fig6e, fig6f,
                  ncol = 3, byrow = FALSE,
                  align = "v")

ggsave(filename = "fig_scripts/egu_talk/fig6.png",
       plot = fig6,
       height = 4.5,
       width = 8,
       units = "in")
