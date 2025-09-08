# Fig S3
# A) model fit for Gardner model


library(coda)
library(readxl)
library(broom.mixed)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

#### A) Predicted vs Observed with WP4 samples ####

# Load posterior predicted values
load("scripts/mod3 - Gardner/coda/coda_pred_hier.Rdata")

sum_pred <- broom.mixed::tidyMCMC(coda_pred, 
                                  conf.int = TRUE, 
                                  conf.method = "HPDinterval") |> 
  rename(pred.upper = conf.high, 
         pred.lower = conf.low, 
         pred.mean = estimate)

# Read in original data and add values
wp4 <- read_csv("data_clean/moisture_release.csv") |> 
  rename(wp = mpa) |> 
  bind_cols(sum_pred[, c(2, 4:5)]) |> 
  mutate(Depth = case_when(depth == 10 ~ "10 cm",
                           depth == 25 ~ "25 cm"),
         coverage = ifelse(log(abs(wp)) <= pred.upper & log(abs(wp)) >= pred.lower, 1, 0))

m1 <- lm(pred.mean ~ log(abs(wp)), data = wp4)
summary(m1) # R2 = 0.963
coefs <- coef(summary(m1))
coefs[2,1]

mean(wp4$coverage) # Coverage of 0.969

# colors
cols_pur <-brewer.pal(7, "PRGn")
display.brewer.pal(7, "PRGn")

figS3 <- wp4 |> 
  ggplot() +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = coefs[2,1], intercept = coefs[1,1],
              linetype = "dashed") +
  geom_errorbar(aes(x = log(abs(wp)),
                    ymin = pred.lower,
                    ymax = pred.upper,
                    color = Depth),
                width = 0,
                alpha = 0.25) +
  geom_point(aes(x = log(abs(wp)), 
                 y = pred.mean, 
                 color = Depth)) +
  geom_text(x = 2, y = -2.5, 
            label = "italic(R^2)==0.963",
            parse = TRUE,
            hjust = 0,
            vjust = 1,
            size = 4) +
  scale_x_continuous(expression(paste("Observed log(|", Psi[soil], "|)")),
                     limits = c(-3.6, 4.6)) +
  scale_y_continuous(expression(paste("Predicted log(", Psi[soil], "|)")),
                     limits = c(-3.6, 4.6)) +
  scale_color_manual(values = cols_pur[2:1]) +
  theme_bw(base_size = 14) +
  coord_equal() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.2, 0.9),
        legend.background = element_blank())

ggsave("fig_scripts/round2/figS3.png",
       figS3,
       height = 4,
       width = 4,
       units = "in")

#### B) Compare S2 and S4 timeseries ####
# Teros 21 dataset during 2023 pulse chase only comes from H3
# Single plot for each treatment
obs <- read_csv("data_clean/swp_teros_daily_daytime.csv",
                locale = locale(tz = "America/Phoenix")) |> 
  filter(trt_s %in% c("S1", "S2", "S3", "S4"),
         period == "morn",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |> 
  mutate(Depth = case_when(depth == "10 cm" ~ "shallow",
                           depth == "25 cm" ~ "25 cm") |> 
           factor(levels = c("shallow", "25 cm")))


pred <- read_csv("data_clean/swp_daily_daytime.csv") |> 
  filter(summer %in% c("S1", "S2", "S3", "S4"),
         period == "morn",
         depth != "75 cm",
         date >= as.Date("2023-08-14", tz = "America/Phoenix"),
         date <= as.Date("2023-09-04", tz = "America/Phoenix")) |> 
  rename(trt_s = summer) |> 
  mutate(Depth = case_when(depth == "0-12 cm" ~ "shallow",
                           depth == "25 cm" ~ "25 cm") |> 
           factor(levels = c("shallow", "25 cm")))

ggplot() +
  geom_point(data = obs,
             aes(x = date, y = mean,
                 color = "instrumented")) +
  geom_point(data = pred,
             aes(x = date, y = mean,
                 color = "modeled")) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  facet_grid(cols = vars(trt_s),
             rows = vars(Depth),
             scales = "free_y",
             space = "free_y") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.15, 0.5))

