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


# Load S1, S2, and S4 LWP data
wp_pd <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "WP",
         period == "predawn")

# Load gs, rerun model, calculate resids
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
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))|> 
  left_join(wp_pd, join_by(date_col, trt_s, ID)) |> 
  rename(predawn = value)

# Check high leverage pd values
outlier <- gst |> 
  relocate(predawn, .after = gs) |> 
  filter(phase == "Phase 1") |> 
  ungroup() |> 
  arrange(predawn) |> 
  slice_head(n = 1)

# High leverage point occurs on 8/26 in H1P2, S2 treatment
# Day of minimal difference between predawn and midday (-1.84 vs. -1.91)
# Even as mean SWP is -0.91. Already disconnected from soil?

gst_remove_outlier <- gst |> 
  anti_join(outlier)

gs.pd.phase <- lme4::lmer(gs ~ phase + predawn*phase + (1|ID), data = gst_remove_outlier)

gst_remove_outlier <- gst_remove_outlier |> 
  ungroup() |> 
  mutate(resids_gs = residuals(gs.pd.phase))

gst_sum <- gst_remove_outlier |>
  group_by(trt_s, date_col) |>
  summarize(gs_m = mean(gs),
            gs_sd = sd(gs),
            pd_m = mean(predawn),
            pd_sd = sd(predawn)) |>
  left_join(swp, by = join_by(trt_s == summer, date_col == date)) |> 
  select(-depth, -period) |> 
  rename(swp_0_10_mean = mean,
         swp_0_10_sd = sd) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))


# Load GPP, rerun both models, calcluate resids
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
         swp_0_10_sd = sd,
         ID = Plot) |> 
  left_join(vpd, join_by(date_col == date)) |> 
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))

et.swp.phase <- lme4::lmer(ET ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)
gpp.swp.phase <- lme4::lmer(GPP ~ phase + swp_0_10_mean*phase + (1|ID), data = gpp)

gpp <- gpp |> 
  ungroup() |> 
  mutate(resids_et = residuals(et.swp.phase),
         resids_gpp = residuals(gpp.swp.phase))

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
  select(-period) |> 
  mutate(phase = if_else(swp_0_10_mean > -1, "Phase 1", "Phase 2"))


#### New figure 7 ####
# Each flux on its own
# Faceted by phase, with color blocks
# Different symbols for leaf vs. plot level
# Accompanying residual plot with VPD, simple linear regression
my_labeller <- as_labeller(c(gs = "g[s]~(mmol~H[2]*O~m^-2~s^-1)",
                             et = "ET~(mmol~H[2]*O~m^-2~s^-1)",
                             gpp = "GPP~(mu*mol~CO[2]~m^-2~s^-1)",
                             `Phase 1` = "Phase~1",
                             `Phase 2` = "Phase~2"),
                           default = label_parsed)
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(6,3)])))


#### gs ####

# Switch to plotable params
model.fixef.results2 <- read_csv("tables/fluxes/gs_PD_cellmeans_KR.csv")

gs_params <- model.fixef.results2 |>
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "PD" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)

cell_means <- read_csv("tables/fluxes/gsresid_VPD_cellmeans_KR.csv")

gs_resid_params <- cell_means |> 
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "VPD" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)



# A
figa <- ggplot() +
  geom_point(data = gst_remove_outlier,
             aes(x = predawn,
                 y = gs),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbarh(data = gst_sum, 
                aes(y = gs_m,
                    xmin = pd_m - pd_sd,
                    xmax = pd_m + pd_sd),
                alpha = 0.5) +
  geom_errorbar(data = gst_sum, 
                aes(x = pd_m,
                    ymin = gs_m - gs_sd,
                    ymax = gs_m + gs_sd),
                alpha = 0.5) +
  geom_point(data = gst_sum,
              aes(x = pd_m,
                 y = gs_m),
             size = 3) +
  geom_abline(data = gs_params,
              aes(slope = -slope, 
                  intercept = intercept,
                  lty = sig)) +
  scale_y_continuous(expression(paste(g[s], " (mmol ", H[2], "O ", m^-2, " ", s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[PD], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_grid2(~phase,
              strip = strip,
              # space = "free_x",
              scales = "free_x") +
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside") +
  guides(shape = "none",
         lty = "none")

figa
# B
figb <- ggplot() +
  geom_point(data = gst_remove_outlier,
             aes(x = D_morn_mean,
                 y = resids_gs,
                 color = phase),
             size = 1.5) +
  geom_abline(data = gs_resid_params,
              aes(slope = slope, 
                  intercept = intercept,
                  color = phase,
                  lty = sig)) +
  scale_y_continuous(expression(paste(g[s], " resids"))) +
  scale_x_continuous("VPD (kPa)") +
  scale_linetype_manual(values = "longdash") +
    scale_color_manual(values = cols_div[c(6,3)]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        # legend.spacing.y = unit(0.05, "cm"),
        legend.margin = margin(t = 0.025, unit = "cm"),
        # legend.position.inside = c(0.5, 0.9),
        legend.position = "top"
        ) +
  guides(lty = "none",
         color = guide_legend(override.aes = list(linetype = c(0, 0)),
                              byrow = TRUE,
                              keywidth = 0.1,
                              keyheight = 0.01,
                              default.unit = "cm"))
figb

row1 <- cowplot::plot_grid(figa, figb, nrow = 1,
                           rel_widths = c(2.25, 1),
                           labels = c("a", "b"))
row1


#### ET ####

# Switch to plotable params
model.fixef.results2 <- read_csv("tables/fluxes/et_SWP_cellmeans_KR.csv")

et_params <- model.fixef.results2 |>
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "SWP" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)

cell_means <- read_csv("tables/fluxes/etresid_VPD_cellmeans_KR.csv")

et_resid_params <- cell_means |> 
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "VPD" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)



# C
figc <- ggplot() +
  geom_point(data = gpp,
             aes(x = swp_0_10_mean,
                 y = ET),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum, 
                aes(x = swp_0_10_mean,
                    ymin = et_m - et_sd,
                    ymax = et_m + et_sd),
                alpha = 0.5) +
  geom_point(data = gpp_sum,
             aes(x = swp_0_10_mean,
                 y = et_m),
             size = 3) +
  geom_abline(data = et_params,
              aes(slope = -slope, 
                  intercept = intercept,
                  lty = sig)) +
  scale_y_continuous(expression(paste("ET (mmol ", H[2], O, " ", m^-2, s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_grid2(~phase,
              strip = strip,
              space = "free_x",
              scales = "free_x") +
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside") +
  guides(shape = "none",
         lty = "none")

# D
figd <- ggplot() +
  geom_point(data = gpp,
             aes(x = D_morn_mean,
                 y = resids_et,
                 color = phase),
             size = 1.5) +
  geom_abline(data = et_resid_params,
              aes(slope = slope, 
                  intercept = intercept,
                  color = phase,
                  lty = sig)) +
  scale_y_continuous("ET resids") +
  scale_x_continuous("VPD (kPa)") +
  scale_linetype_manual(values = "solid") +
  scale_color_manual(values = cols_div[c(6,3)]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        # legend.spacing.y = unit(0.05, "cm"),
        legend.margin = margin(t = 0.025, unit = "cm"),
        # legend.position.inside = c(0.5, 0.9),
        legend.position = "top"
  ) +
  guides(lty = "none",
         color = guide_legend(override.aes = list(linetype = c(0, 0)),
                              byrow = TRUE,
                              keywidth = 0.1,
                              keyheight = 0.01,
                              default.unit = "cm"))

row2 <- cowplot::plot_grid(figc, figd, nrow = 1,
                           rel_widths = c(2.25, 1),
                           labels = c("c", "d"))
row2

#### GPP ####

# Switch to plotable params
model.fixef.results2 <- read_csv("tables/fluxes/gpp_SWP_cellmeans_KR.csv")

gpp_params <- model.fixef.results2 |>
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "SWP" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)

cell_means <- read_csv("tables/fluxes/gppresid_VPD_cellmeans_KR.csv")

gpp_resid_params <- cell_means |> 
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "VPD" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)



# E
fige <- ggplot() +
  geom_point(data = gpp,
             aes(x = swp_0_10_mean,
                 y = GPP),
             size = 1.5,
             alpha = 0.25) +
  geom_errorbar(data = gpp_sum, 
                aes(x = swp_0_10_mean,
                    ymin = gpp_m - gpp_sd,
                    ymax = gpp_m + gpp_sd),
                alpha = 0.5) +
  geom_point(data = gpp_sum,
             aes(x = swp_0_10_mean,
                 y = gpp_m),
             size = 3) +
  geom_abline(data = gpp_params,
              aes(slope = -slope, 
                  intercept = intercept,
                  lty = sig)) +
  scale_y_continuous(expression(paste("GPP (", mu, "mol ", CO[2], " ", m^-2, s^-1, ")"))) +
  scale_x_reverse(expression(paste(Psi[soil], " (MPa)"))) +
  scale_linetype_manual(values = c("longdash", "solid")) +
  facet_wrap2(~phase,
              strip = strip,
              # space = "free_x",
              scales = "free_x") +  
  force_panelsizes(cols = c(1,1.75)) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.background = element_blank(),
        strip.placement = "outside") +
  guides(shape = "none",
         lty = "none")

# F
figf <- ggplot() +
  geom_point(data = gpp,
             aes(x = D_morn_mean,
                 y = resids_gpp,
                 color = phase),
             size = 1.5) +
  geom_abline(data = gpp_resid_params,
              aes(slope = slope, 
                  intercept = intercept,
                  color = phase,
                  lty = sig)) +
  scale_y_continuous("GPP resids") +
  scale_x_continuous("VPD (kPa)") +
  scale_linetype_manual(values = "longdash") +
  scale_color_manual(values = cols_div[c(6,3)]) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        # legend.spacing.y = unit(0.05, "cm"),
        legend.margin = margin(t = 0.025, unit = "cm"),
        # legend.position.inside = c(0.5, 0.9),
        legend.position = "top"
  ) +
  guides(lty = "none",
         color = guide_legend(override.aes = list(linetype = c(0, 0)),
                              byrow = TRUE,
                              keywidth = 0.1,
                              keyheight = 0.01,
                              default.unit = "cm"))

row3 <- cowplot::plot_grid(fige, figf, nrow = 1,
                           rel_widths = c(2.25, 1),
                           labels = c("e", "f"))
row3


#### Assemble panels for fig 7 ####


fig_7 <- cowplot::plot_grid(figa, figb, 
                            figc, figd, 
                            fige, figf,
                            nrow = 3,
                            align = "hv", axis = "lb",
                            rel_widths = c(2.25, 1),
                            labels = "auto")
fig_7

ggsave(filename = "fig_scripts/round2/fig7.png",
       plot = fig_7,
       height = 7,
       width = 8,
       units = "in")

##### Alternative residual plot for GPP ####

#Load par resids
cell_means <- read_csv("tables/fluxes/gppresid_PAR_cellmeans_KR.csv")

gpp_resid_params <- cell_means |> 
  separate(parameter, into = c("phase", "term"), sep = ":") |>
  # uncount(c(1,1,2,1,1,2)) |>
  mutate(type = case_when(term == "PAR" ~ "slope",
                          .default = "intercept"),
         sig = ifelse(pval < 0.05, TRUE, FALSE),
         sig = ifelse(type == "slope", sig, NA)) |>
  select(-ci, -den.df, -tstat, -pval, -term) |>
  pivot_wider(names_from = type, 
              values_from = c(estimate, sig)) |>
  select(-sig_intercept) |>
  rename(sig = sig_slope, intercept = estimate_intercept, slope = estimate_slope)


# G
figg <- ggplot() +
  geom_point(data = gpp,
             aes(x = Par,
                 y = resids_gpp,
                 color = phase),
             size = 1.5) +
  geom_abline(data = gpp_resid_params,
              aes(slope = slope, 
                  intercept = intercept,
                  color = phase,
                  lty = sig)) +
  scale_y_continuous("GPP resids") +
  scale_x_continuous(expression(paste("PAR (", mu, "mol ", " ", m^-2, s^-1, ")"))) +
  scale_linetype_manual(values = "longdash") +
  scale_color_manual(values = cols_div[c(6,3)]) +
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank()) +
  guides(color = "none", lty = "none")


ggsave(filename = "fig_scripts/round2/figS_.png",
       plot = figg,
       height = 3,
       width = 3,
       units = "in")
