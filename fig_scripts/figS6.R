# Fig S6
# GPP-SWP model residuals with PAR
# In case reviewers question why plot against VPD
library(tidyverse)

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



##### Alternative residual plot for GPP ####
# Plotting vars
my_labeller <- as_labeller(c(gs = "g[s]~(mmol~H[2]*O~m^-2~s^-1)",
                             et = "ET~(mmol~H[2]*O~m^-2~s^-1)",
                             gpp = "GPP~(mu*mol~CO[2]~m^-2~s^-1)",
                             `Phase 1` = "Phase~1",
                             `Phase 2` = "Phase~2"),
                           default = label_parsed)
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_x = elem_list_rect(fill = c(cols_div[c(6,3)])))


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


# Figure
figS6 <- ggplot() +
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
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank()) +
  guides(color = "none", lty = "none")


ggsave(filename = "fig_scripts/round2/figS6.png",
       plot = figS6,
       height = 3,
       width = 3,
       units = "in")
