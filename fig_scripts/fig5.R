# Bivariate plots divided by Phase 1/2
# Uses threshold of SWP = -1 MPa
# And so combines data from all treatments

library(coda)
library(tidyverse)
library(broom.mixed)
library(RColorBrewer)
library(MASS) # masks dplyr::select
library(broom)
library(ggh4x)
library(cowplot)

# Load coda_params
load("scripts/mod4 - piecewise/coda/coda_params_mod1.Rdata")

# Summarize
param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Obtain posterior summary of changepoints
cps <- param_sum |> 
  filter(grepl("^mu\\.cp", term))
cp1 <- cps[1,]
cp2 <- cps[2,]
# Load S4 data
wp <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP")

# Load VPD and SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(wp$date_col),
         date <= max(wp$date_col),
         depth == "0-12 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         SWP_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)

vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "inside",
         period %in% c("PD", "MD"),
         !is.na(mean)) |>  
  dplyr::select(-location, -sd) |> 
  mutate(period = case_when(period == "PD" ~ "predawn",
                            period == "MD" ~ "midday")) |> 
  rename(Dmean = mean)

# Combine
wp_all <- wp |> 
  left_join(swp, by = join_by(date_col == date,
                              trt_s)) |> 
  left_join(vpd, by = join_by(date_col == date, 
                              period))

# Determine phase by filtering for above and below SWP = -1 MPa 
# then rejoin
p1 <- wp_all |> 
  filter(SWP_1 >= -1) |> 
  mutate(phase = "Phase 1")
p2 <- wp_all |> 
  filter(SWP_1 < -1) |> 
  mutate(phase = "Phase 2")

wp_phase <- bind_rows(p1, p2) |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")))

#### Test lms with model comparison ####

# Used backward selection with AIC as guidance
# 3 out of 4 models used the additive model 
# (different ints but same slope)
# So use for all 4 panels?

m1_start <- lm(value ~ Dmean*period2, data = wp_phase |> 
           filter(phase == "Phase 1"))
m1_step <- stepAIC(m1_start, scope = list(lower = ~period2),
                   direction = "backward",
                   trace = 2)
m1_step$anova
m1 <- lm(value ~ Dmean + period2, data = wp_phase |> 
           filter(phase == "Phase 1"))
summary(m1) # all 3 are significant
cf1 <- coef(m1)

m2_start <- lm(value ~ Dmean*period2, data = wp_phase |> 
           filter(phase == "Phase 2"))
m2_step <- stepAIC(m2_start, scope = list(lower = ~period2),
                   direction = "backward",
                   trace = 2)
m2_step$anova
m2 <- lm(value ~ Dmean + period2, data = wp_phase |> 
           filter(phase == "Phase 2"))

summary(m2) # only single intercept + Dmean slope
cf2 <- coef(m2)

cf_D <- data.frame(period2 = rep(c("PD", "MD"), 2),
           phase = rep(c("Phase 1", "Phase 2"), each = 2),
           int = c(cf1[1], cf1[1] + cf1[3], cf2[1], cf2[1]),
           slope = c(cf1[2], cf1[2], cf2[2], cf2[2])) |> 
  mutate(period2 = factor(period2, levels = c("PD", "MD")))



m3_start <- lm(value ~ SWP_1*period2, data = wp_phase |> 
           filter(phase == "Phase 1"))
m3_step <- stepAIC(m3_start, scope = list(lower = ~period2),
                   direction = "backward",
                   trace = 1)
m3_step$anova
m3 <- lm(value ~ SWP_1 + period2, data = wp_phase |> 
                 filter(phase == "Phase 1"))
summary(m3)
cf3 <- coef(m3)

m4_start <- lm(value ~ SWP_1*period2, data = wp_phase |> 
           filter(phase == "Phase 2"))
m4_step <- stepAIC(m4_start, scope = list(lower = ~period2),
                   direction = "backward",
                   trace = 2)
m4_step$anova
m4 <- lm(value ~ SWP_1 + period2, data = wp_phase |> 
                 filter(phase == "Phase 2"))
summary(m4) # intercept not significant
cf4 <- coef(m4)

cf_SWP <- data.frame(period2 = rep(c("PD", "MD"), 2),
                   phase = rep(c("Phase 1", "Phase 2"), each = 2),
                   int = c(cf3[1], cf3[1] + cf3[3], 0, 0 + cf4[3]),
                   slope = c(0, 0, cf4[2], cf4[2])) |> 
  mutate(period2 = factor(period2, levels = c("PD", "MD")))

# Model comparison 

mc_D <- broom::glance(m1) |> 
  bind_rows(broom::glance(m2)) |> 
  mutate(phase = c("Phase 1", "Phase 2"),
         r2 = paste0("italic(R^2) == ", 
                     round(adj.r.squared, 3)))

mc_SWP <- broom::glance(m3) |> 
  bind_rows(broom::glance(m4)) |> 
  mutate(phase = c("Phase 1", "Phase 2"),
         r2 = paste0("italic(R^2) == ", 
                     round(adj.r.squared, 3)))


#### Assemble panels ####

cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")

cols_div <- brewer.pal(7, "Accent")
display.brewer.pal(7, "Accent")

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])))
strip <- strip_themed(background_x = elem_list_rect(fill = cols_div[c(1,4)]))

fig5a <- wp_phase |> 
  ggplot() +
  geom_point(aes(x = Dmean, y = value, color = period2)) +
  geom_abline(data = cf_D,
              aes(slope = slope, intercept = int,
                  color = period2)) +
  geom_text(data = mc_D,
            aes(x = -1, y = -5.5, label = r2),
            parse = TRUE,
            hjust = 0) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_x_continuous("VPD (kPa)") +
  scale_color_manual(values = cols_gn[4:3], 
                     label = labs) +
  facet_wrap2(~phase, strip = strip) +
             # scales = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.4),
        legend.background = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 0))))

fig5b <- wp_phase |> 
  ggplot() +
  geom_point(aes(x = SWP_1, y = value, color = period2)) +
  geom_abline(data = cf_SWP,
              aes(slope = slope, intercept = int,
                  color = period2)) +
  geom_text(data = mc_SWP,
            aes(x = c(-1, -2), y = -5.5, label = r2),
            parse = TRUE,
            hjust = 0) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)"))) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_color_manual(values = cols_gn[4:3]) +
  facet_wrap2(~phase, strip = strip,
              scales = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank()) +
  guides(color = "none")

fig5 <- plot_grid(fig5a, fig5b, 
          ncol = 1,
          align = "v",
          labels = "auto")

ggsave(filename = "fig_scripts/fig5.png",
       plot = fig5,
       height = 4.5,
       width = 6,
       units = "in")

