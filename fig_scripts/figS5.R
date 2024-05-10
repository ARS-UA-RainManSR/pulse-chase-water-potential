# Model fit for RWC-WP model with imputed RWC from RWC_ind
library(coda)
library(tidyverse)
library(broom.mixed)


# load data files
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
         phase = if_else(SWP_1 <= -1, "Phase 2", "Phase 1")) 

# Select actual data for model - only Phase 2
dat <- hyp_wide |>
  filter(phase == "Phase 2",
         !is.na(RWC_ind)) |> # 76 observations of RWC_ind and WP, but 61 observations of RWC
  select(-ID_long:-WPI2, -CNDI) |>
  mutate(logitRWC = log(RWC/(1-RWC))) # create a logit-transformed variable

# Loas predicted WP
load(file = "scripts/mod6 - rwc/coda/coda_pred_wp.Rdata")



pred_sum <- tidyMCMC(coda_pred,
                     conf.int = TRUE,
                     conf.method = "HPDinterval") %>%
  rename(pred.mean = estimate,
         pred.lower = conf.low,
         pred.upper = conf.high)

preds <- cbind.data.frame(dat, pred_sum) |>
  mutate(missing = if_else(is.na(RWC), "Imputed RWC", "Observed RWC") |>
           factor(levels = c("Observed RWC", "Imputed RWC")))

table(preds$missing) # 15 missing RWC's
# Linear fit
m1 <- lm(-1*exp(pred.mean) ~ WP, data = filter(preds, missing == "Observed RWC"))
s1 <- summary(m1)
s1 # R2 = 0.7767

m2 <- lm(-1*exp(pred.mean) ~ WP, data = filter(preds, missing == "Imputed RWC"))
s2 <- summary(m2)
s2 # R2 = 0.8992

# Make df of model parameters
fit_df <- data.frame(missing = c("Observed RWC", "Imputed RWC"),
                     slope = c(s1$coefficients[2,1], s2$coefficients[2,1]),
                     int = c(s1$coefficients[1,1], s2$coefficients[1,1]),
                     adj.R2 = c(s1$adj.r.squared, s2$adj.r.squared)) |>
  mutate(lab1 = paste0("R^2==", round(adj.R2, 3)),
         lab2 = paste0("Bias==", round(slope, 3)),
         missing = factor(missing, levels = c("Observed RWC", "Imputed RWC")))


# Plot the fit
cols_gn <- brewer.pal(4, "Paired")
display.brewer.pal(4, "Paired")

labs <- c(lapply(c("PD", "MD"), function(i) bquote(Psi[.(i)])))

fig <- preds |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0,) +
  geom_abline(data = fit_df,
              aes(slope = slope, intercept = int),
              lty = "dashed") +
  geom_errorbar(aes(x = WP,
                    ymin = -1*exp(pred.lower),
                    ymax = -1*exp(pred.upper),
                    color = period2),
                alpha = 0.5) +
  geom_point(aes(x = WP, y = -1*exp(pred.mean), 
                 color = period2,
                 shape = missing),
             stroke = 1) +
  geom_text(data = fit_df,
            aes(x = -.5, y = -8.5, label = lab1),
            parse = TRUE,
            hjust = 1) +
  geom_text(data = fit_df,
            aes(x = -.5, y = -9.5, label = lab2),
            parse = TRUE,
            hjust = 1) +
  scale_x_continuous(expression(paste("Observed ", Psi, " (MPa)")),
                     limits = c(-10, 0)) +
  scale_y_continuous(expression(paste("Predicted ", Psi, " (MPa)")),
                     limits = c(-10, 0)) +
  scale_color_manual(values = cols_gn[4:3], 
                     label = labs) +  
  scale_shape_manual(values = c(16, 21)) +
  facet_wrap(~missing) +
  coord_equal() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1, 0.8)) +
  guides(shape = "none")

ggsave(filename = "fig_scripts/figS5.png",
       plot = fig,
       height = 3.5,
       width = 6,
       units = "in")
