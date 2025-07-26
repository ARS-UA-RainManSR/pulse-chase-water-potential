# Associate fluxes against SWP or PD and maybe VPD

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggh4x)


# Load SWP
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         depth == "0-10 cm",
         summer != "S3",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |>
  mutate(trt_label = case_when(summer == "S1" ~ "P3.5",
                               summer == "S2" ~ "P7",
                               summer == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# Load VPD
vpd <- read_csv("data_clean/vpd_daily_daytime.csv") |> 
  filter(location == "inside",
         period == "morn",
         !is.na(mean)) |>  
  dplyr::select(-location) |> 
  rename(D_morn_mean = mean, D_morn_sd = sd)

# Load gs
gst <- read_csv(file = "data_clean/gs_leaftemp.csv",
                locale = locale(tz = "America/Phoenix")) |>
  rename(ID = hp) |>
  mutate(date_col = as.Date(date)) |>
  group_by(date_col, trt_s, ID) |>
  summarize(gs = mean(conductance)) |> 
  left_join(swp, by = join_by(trt_s == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period)


gst_sum <- gst |>
  group_by(trt_s, date_col) |>
  summarize(gs_m = mean(gs),
            gs_sd = sd(gs)) |>
  left_join(swp, by = join_by(trt_s == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period)

# Load GPP
gpp <- read_csv("data/plotgas2023.csv") |>
  mutate(date_col = lubridate::mdy(Date,
                                   tz = "America/Phoenix") |> 
           as.Date(),
         ER2 = ifelse(ER < 0, 0, ER), # Restrict ER to positive values
         GPP = -NEE + ER2,
         house = str_extract(Plot, "H[0-9]")) |> 
  filter(date_col >= min(swp$date),
         date_col <= max(swp$date)) |> 
  left_join(swp, by = join_by(PT == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period)


gpp_sum <- gpp |>
  group_by(PT, date_col) |>
  summarize(gpp_m = mean(GPP),
            gpp_sd = sd(GPP),
            et_m = mean(ET),
            et_sd = sd(ET)) |> 
  left_join(swp, by = join_by(PT == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period)


# Energy vs water limited
# Create single dataframe to facet by variable and phase
cols_div <- brewer.pal(7, "Spectral")
display.brewer.pal(7, "Spectral")

all_sum <- gst_sum |> 
  full_join(gpp_sum, by = join_by(trt_s == PT, date_col, trt_label,
                                  swp_0_10_mean, swp_0_10_sd, 
                                  D_morn_mean, D_morn_sd)) |> 
  pivot_longer(cols = c("gs_m", "gs_sd",
                        "gpp_m", "gpp_sd",
                        "et_m", "et_sd"),
               names_to = c("flux", "variable"),
               names_pattern = "(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = variable,
              values_from = value) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"),
         flux = factor(flux, levels = c("gs", "et", "gpp")),
         level = if_else(flux == "gs", "leaf", "plot"))

my_labeller <- as_labeller(c(gs = "g[s]~(mmol~H[2]*O~m^-2~s^-1)",
                             et = "ET~(mmol~H[2]*O~m^-2~s^-1)",
                             gpp = "GPP~(mu*mol~CO[2]~m^-2~s^-1)",
                             `Phase 1` = "Phase~1",
                             `Phase 2` = "Phase~2"),
                           default = label_parsed)
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(6,3)])))

cols_bl <- brewer.pal(9, "Blues")[c(9,7,4)]

# first plot with only VPD shown (no treatment)
fig_7 <- all_sum |> 
  ggplot() +
  geom_errorbar(aes(x = swp_0_10_mean,
                    ymin = m - sd,
                    ymax = m + sd,
                    color = D_morn_mean),
                alpha = 0.5) +
  geom_point(aes(x = swp_0_10_mean,
                 y = m,
                 color = D_morn_mean,
                 shape = level),
             size = 3) +
  scale_color_gradient("VPD", low = "cornflowerblue", high = "coral") +
  scale_shape_manual(values = c(16, 15)) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  facet_grid2(rows = vars(flux),
              cols = vars(phase),
              strip = strip,
              scales = "free",
              # space = "free_x",
              labeller = my_labeller,
              switch = "y") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.position = "right") +
  guides(shape = "none")

fig_7

ggsave(filename = "fig_scripts/round2/fig7.png",
       plot = fig_7,
       height = 7,
       width = 6,
       units = "in")


#### Try lm with SWP and VPD for phase 1 ####

#### gs ####

gst |> 
  # filter(swp_0_10_mean < -1) |>
  ggplot(aes(x = swp_0_10_mean, y = gs)) +
  geom_point(aes(color = D_morn_mean)) +
  scale_color_gradient(high = "coral",
                       low = "cornflowerblue")
gst_phase1 <- gst |> 
  filter(swp_0_10_mean < -1)

m1 <- lm(gs ~ swp_0_10_mean, data = gst_phase1)
summary(m1)

gst_phase1$resids <- resid(m1)

m1_resid <- lm(resids ~ D_morn_mean, data = gst_phase1)
summary(m1_resid)

# For gs, SWP is signficant in phase 1 but not phase 2
# In both phases VPD is NOT significant on the residuals

#### ET ####
gpp |> 
  # filter(swp_0_10_mean < -1) |>
  ggplot(aes(x = swp_0_10_mean, y = ET)) +
  geom_point(aes(color = D_morn_mean)) +
  scale_color_gradient(high = "coral",
                       low = "cornflowerblue")
gpp_phase1 <- gpp |> 
  filter(swp_0_10_mean < -1)

m2 <- lm(ET ~ swp_0_10_mean, data = gpp_phase1)
summary(m2)

gpp_phase1$resids <- resid(m2)

m2_resid <- lm(resids ~ D_morn_mean, data = gpp_phase1)
summary(m2_resid)

# For ET, SWP is signficant in phase 1 but not phase 2
# In both phases VPD IS significant on the residuals

#### GPP ####

gpp |> 
  # filter(swp_0_10_mean < -1) |>
  ggplot(aes(x = swp_0_10_mean, y = GPP)) +
  geom_point(aes(color = D_morn_mean)) +
  scale_color_gradient(high = "coral",
                       low = "cornflowerblue")

gpp |> 
  # filter(swp_0_10_mean < -1) |>
  ggplot(aes(x = swp_0_10_mean, y = GPP)) +
  geom_point(aes(color = Tav)) +
  scale_color_gradient(high = "coral",
                       low = "cornflowerblue")

# Much more of a fan shape to the data
gpp_phase1 <- gpp |> 
  filter(swp_0_10_mean > -1)

m3 <- lm(GPP ~ swp_0_10_mean, data = gpp_phase1)
summary(m3)

gpp_phase1$resids <- resid(m3)

m3_resid <- lm(resids ~ D_morn_mean, data = gpp_phase1)
summary(m3_resid)

# For GPP, SWP is marginally signficant in phase 1 and significant phase 2
# In both phases Tav IS significant on the residuals (but VPD is not)

# Try for the whole range (no dividing into phases)
m4 <- lm(GPP ~ swp_0_10_mean, data = gpp)
summary(m4)

gpp$resids <- resid(m4)

m4_resid <- lm(resids ~ Tav, data = gpp)
summary(m4_resid)

# Across the whole range of GPP, SWP is significant
# Tav IS significant on the residuals (but VPD is not)

cor(gpp$Par, gpp$D_morn_mean)
cor(gpp$swp_0_10_mean, gpp$D_morn_mean)
cor(gpp$Tav, gpp$Par)
cor(gpp$Tav, gpp$D_morn_mean)

# then try with treatment and VPD
fig_7new <- all_sum |> 
  ggplot() +
  geom_errorbar(aes(x = swp_0_10_mean,
                    ymin = m - sd,
                    ymax = m + sd,
                    color = D_morn_mean),
                alpha = 0.5) +
  geom_point(aes(x = swp_0_10_mean,
                 y = m,
                 color = D_morn_mean,
                 shape = trt_label),
             size = 3) +
  scale_shape_manual("Treatment", values = c(19, 17, 15)) +
  scale_color_gradient("VPD", low = "cornflowerblue", high = "coral") +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  facet_grid2(rows = vars(flux),
              cols = vars(phase),
              strip = strip,
              scales = "free",
              space = "free_x",
              labeller = my_labeller,
              switch = "y") +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title.y = element_blank(),
        strip.text = element_text(size = 10)) 

ggsave(filename = "fig_scripts/round2/fig7_v2.png",
       plot = fig_7new,
       height = 6,
       width = 8,
       units = "in")

# Modeling Phase 1
phase1_flux <- gpp |> 
  filter(swp_0_10_mean >= -1)

phase1_gs <- gst |> 
  filter(swp_0_10_mean >= -1)


gpp |> 
  filter(swp_0_10_mean >= -1) |> 
  ggplot(aes(x = Par)) +
  geom_point(aes(y = GPP,
                 color = D_midday_mean,
                 shape = house))

m1 <- lm(GPP ~ scale(swp_0_10_mean) * scale(D_midday_mean), data = phase1_flux)
summary(m1)

m2 <- lm(gs ~ scale(swp_0_10_mean) * scale(D_midday_mean), data = phase1_gs)
summary(m2)


gpp |> 
  filter(swp_0_10_mean >= -1) |> 
  ggplot(aes(x = swp_0_10_mean)) +
  geom_point(aes(y = ET,
                 color = D_midday_mean,
                 shape = house))
m3 <- lm(ET ~ scale(swp_0_10_mean) * scale(Par), data = phase1_flux)
summary(m3)

cor(gpp$swp_0_10_mean, gpp$D_midday_mean)


