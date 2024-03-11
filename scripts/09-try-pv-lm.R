# PV curve for S4 only
# Separate by SWP threshold of -1 MPa

library(tidyverse)

# Read in data, make wide
wide <- read_csv("data_clean/wp_rwc_long.csv") |>
  pivot_wider(names_from = variable,
              values_from = value) |>
  filter(trt_s == "S4") |>
  mutate(period = case_when(period == "predawn" ~ "PD",
                            period == "midday" ~ "MD") |>
           factor(levels = c("PD", "MD")),
         days_since_pulse = if_else(date_col == as.Date("2023-09-04"),
                                    21, days_since_pulse))

# Add in SWP
swp <- read_csv("data_clean/swp_daily_daytime.csv") |>
  filter(depth == "0-12 cm",
         period == "morn",
         summer == "S4") |>
  select(-depth, -period, -summer) |>
  rename(SWP_1 = mean)

# Summarize predawns only, join with swp
pd <- wide |>
  filter(period == "PD",
         !is.na(RWC),
         RWC < 0.95) |>
  mutate(x = 1-RWC,
         y = -1/WP) |>
  left_join(swp, by = join_by('date_col' == 'date'))

# Fit linear models to Phase 1 and Phase 2
p1 <- pd |>
  filter(SWP_1 > -1)
p2 <- pd |>
  filter(SWP_1 <= -1)

m1 <- lm(y ~ x, data = p1)
summary(m1)

m2 <- lm(y ~ x, data = p2)
summary(m2)

cf <- data.frame(phase = c("Phase 1", "Phase 2"),
                 slope = c(coef(m1)[2], coef(m2)[2]),
                 int = c(coef(m1)[1], coef(m2)[1]))

ggplot() +
  geom_point(data = pd,
             aes(x = 1-RWC, y = -1/WP)) +
  geom_abline(data = cf,
              aes(slope = slope, intercept = int,
                  color = phase)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())


