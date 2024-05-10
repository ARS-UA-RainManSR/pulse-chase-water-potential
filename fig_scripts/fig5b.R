# New fig 6?
# RWC and WP by phase, with RWC_ind
library(tidyverse)
library(coda)
library(broom.mixed)
library(RColorBrewer)
library(ggh4x)
library(cowplot)

# Load data
# indices from hyperspectral
hyp_ind <- read_csv("data_clean/hyp_indices.csv") |> 
  mutate(date_col = as.Date(date_col, "%m/%d/%Y", tz = "America/Phoenix"))

# Load SWP data
swp <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(period == "morn",
         date >= min(hyp_ind$date_col),
         date <= max(hyp_ind$date_col),
         depth == "0-12 cm",
         summer != "S3") |> 
  rename(trt_s = summer,
         SWP_1 = mean) |> 
  dplyr::select(-period, -sd, -depth)


# Load rwc-wp data, pivot wider, 
# join with hyperspec and swp data
# define phases
hyp_wide <- read_csv("data_clean/wp_rwc_long.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD"))) |>
  select(-time_wp, -notes_wp, -time_rwc, -notes_rwc) |> 
  pivot_wider(names_from = variable,
              values_from = value) |>
  # Join hyperspectral
  left_join(hyp_ind) |>
  # Join SWP
  left_join(swp, by = join_by("date_col" == "date", "trt_s")) |>
  # dummy vector to connect the points
  # label with phase determined by SWP < -1 MPa
  mutate(pulse_num2 = if_else(pulse_num == 8, 7, pulse_num),
         pulse_num2 = if_else(pulse_num2 == 4, 3, pulse_num2),
         phase = if_else(SWP_1 <= -1, "Phase 2", "Phase 1")) |>
  group_by(phase) |>
  mutate(RWC_ind_scale = scale(RWC_ind))


# Load predicted RWC
load(file = "scripts/mod6 - rwc/coda/coda_pred_rwc.Rdata")

rwc_sum <- tidyMCMC(coda_rwc,
                    conf.int = TRUE,
                    conf.method = "HPDinterval") %>%
  rename(rwc.mean = estimate,
         rwc.lower = conf.low,
         rwc.upper = conf.high)

# Load coda
load("scripts/mod6 - rwc/coda/coda_params.Rdata")

param_sum <- tidyMCMC(coda_params,
                      conf.int = TRUE,
                      conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

# Create beta parameters
B_df <- param_sum |>
  filter(grepl("^B", term)) |>
  mutate(var = c("Intercept", "Slope")) |>
  select(var, pred.mean) |>
  pivot_wider(names_from = var, 
              values_from = pred.mean) |>
  mutate(phase = "Phase 2")

# Create predicted line for Phase 2
rwc_from_ind <- function(rwc_ind) {
  logitRWC = B_df$Intercept + B_df$Slope * rwc_ind
  muRWC = exp(logitRWC)/(1 + exp(logitRWC))
  return(muRWC)
}

rwc_pred <- data.frame(rwc_ind = seq(-3, 3, by = 0.01)) |>
  mutate(muRWC = rwc_from_ind(rwc_ind),
         phase = "Phase 2") 

# Estimate missing RWC
imputeRWC_df <- hyp_wide |>
  filter(phase == "Phase 2") |>
  bind_cols(rwc_sum) |>
  filter(is.na(RWC)) |>
  mutate(logitrwc.upper = log(rwc.upper / (1-rwc.upper)),
         logitrwc.lower = log(rwc.lower / (1-rwc.lower)))

# 32 missing RWC overall, 15 missing from Phase 2
cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")

cols_div <- brewer.pal(7, "Spectral")
display.brewer.pal(7, "Spectral")

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])))
strip <- strip_themed(background_x = elem_list_rect(fill = cols_div[c(6,3)]))

fig_b <-
  ggplot() +
  geom_line(data = rwc_pred,
            aes(x = rwc_ind, y = muRWC)) + 
  geom_point(data = hyp_wide, 
             aes(x = RWC_ind_scale, 
                 y = RWC,
                 color = period2)) +
  geom_errorbar(data = imputeRWC_df,
                aes(x = RWC_ind_scale,
                    ymax = rwc.upper,
                    ymin = rwc.lower,
                    color = period2),
                alpha = 0.5,
                width = 0,
                show.legend = FALSE) +
  geom_point(data = imputeRWC_df,
             aes(x = RWC_ind_scale,
                 y = rwc.mean,
                 color = period2),
             shape = 21,
             size = 2,
             stroke = 1) +
  facet_wrap2(~phase, strip = strip) +
  scale_x_continuous(expression(paste(RWC[ind], " (scaled)"))) +
  scale_y_continuous(expression(paste(RWC, " (g", " ", g^-1, ")"))) +
  scale_color_manual(values = cols_gn[4:3]) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.background = element_blank()) +
  guides(color = guide_legend(override.aes = 
                                list(shape = c(16, 16))))

##### Plot WP vs. RWC ####
# Create Gardener parameters
ab_df <-
  param_sum |>
  filter(grepl("log\\.a", term) |
           grepl("inv\\.b", term)) |>
  separate(term, into = c("var", "temp", "period")) |>
  mutate(term = paste0(var, ".", temp)) |>
  relocate(term) |>
  select(-var, -temp, -std.error, -pred.lower, -pred.upper) |>
  mutate(period2 = case_when(period == 1 ~ "PD",
                             period == 2 ~ "MD")) |>
  pivot_wider(names_from = term,
              values_from = pred.mean) |>
  mutate(phase = "Phase 2")

# Create predicted line for Phase 2
gard <- function(rwc, tod) {
  parms <- ab_df |>
    filter(period2 == tod)
  
  logWP = -1*parms$inv.b * (log(rwc) - parms$log.a)
  absWP = exp(logWP)
  return(absWP)
}

gard_pred <- data.frame(rwc = seq(0.25, 1, by = 0.01)) |>
  mutate(PD = gard(rwc, "PD"),
         MD = gard(rwc, "MD")) |>
  pivot_longer(-rwc,
               names_to = "period2",
               values_to = "absWP") |>
  mutate(WP = -1 * absWP,
         phase = "Phase 2")


# Dataframe of imputed RWC and modeled WP
imputeWP_df <- hyp_wide |>
  filter(phase == "Phase 2") |>
  bind_cols(rwc_sum) |>
  # bind_cols(wp_sum) |>
  filter(is.na(RWC))

# 32 missing RWC overall, 15 missing from Phase 2
cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")

cols_div <- brewer.pal(7, "Spectral")
display.brewer.pal(7, "Spectral")

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])))
strip <- strip_themed(background_x = elem_list_rect(fill = cols_div[c(6,3)]))


fig_a <-
  ggplot() +
  geom_line(data = gard_pred,
            aes(x = rwc, y = WP, 
                color = period2)) + 
  geom_point(data = hyp_wide,
             aes(x = RWC, y = WP,
                 color = period2)) +
  geom_errorbarh(data = imputeWP_df,
                 aes(y = WP,
                     xmax = rwc.upper,
                     xmin = rwc.lower,
                     color = period2),
                 alpha = 0.5) +
  # geom_errorbar(data = imputeWP_df,
  #                aes(x = rwc.mean,
  #                    ymax = WP.upper,
  #                    ymin = WP.lower,
  #                    color = period2),
  #                alpha = 0.25) +
  geom_point(data = imputeWP_df,
             aes(x = rwc.mean,
                 y = WP,
                 color = period2),
             shape = 21, 
             size = 2,
             stroke = 1) +
  facet_wrap2(~phase, strip = strip) +
  scale_color_manual(values = cols_gn[4:3], 
                       label = labs) +  
  scale_x_continuous(expression(paste(RWC, " (g", " ", g^-1, ")"))) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  guides(color = "none")

fig5b <- plot_grid(fig_b, fig_a, ncol = 1,
                   align = "v",
                   labels = "auto")

ggsave(filename = "fig_scripts/fig5b.png",
       plot = fig5b,
       height = 4.5,
       width = 6,
       units = "in")

