# Supplemental figure comparing RWC and RWCind

# Comparison of RWC with multiple hyperspec water indices
# Use all treatments but subdivide in to Phase 1 and Phase 2

library(tidyverse)
library(ggh4x)
library(cowplot)
library(RColorBrewer)

# Read in long data with indices
all <- read_csv("data_clean/spectra_ind_wp_rwc.csv") |> 
  mutate(period2 = case_when(period == "predawn" ~ "PD",
                             period == "midday" ~ "MD") |> 
           factor(levels = c("PD", "MD")),
         trt_label = case_when(trt_s == "S1" ~ "P3.5",
                               trt_s == "S2" ~ "P7",
                               trt_s == "S4" ~ "P21") |>
           factor(levels = c("P3.5", "P7", "P21")))

# quick plot
all |> 
  ggplot(aes(x = RWC_ind, y = RWC)) +
  geom_point(aes(color = period)) +
  scale_x_reverse() +
  facet_wrap(~trt_label)


# Check if period is significant
summary(lm(RWC ~ RWC_ind*period, data = filter(all, trt_label == "P3.5")))
summary(lm(RWC ~ RWC_ind, data = filter(all, trt_label == "P7")))
summary(lm(RWC ~ RWC_ind*period, data = filter(all, trt_label == "P21")))

filter(all, trt_label == "P7") |> 
  ggplot(aes(x = RWC_ind, y = RWC)) +
  geom_point(aes(color = period)) +
  geom_abline(slope = -2.91, intercept = 2.70, color = "red") +
  geom_abline(slope = -2.91+1.374, intercept = 2.70-0.845, color = "blue") +
  geom_abline(slope = -2.417, intercept = 2.41, color = "black")

summary(lm(RWC ~ RWC_ind*period, data = filter(all, trt_label == "P21")))

# Repeated lms for each treatment (no need to account fo period)

lm_params<- all |>
  nest_by(trt_label) |>
  mutate(model = list(lm(RWC ~ RWC_ind, data = data))) |>
  summarize(broom::glance(model),
            slope = coef(summary(model))[2,1],
            intercept = coef(summary(model))[1,1]) |>
  mutate(sig = if_else(p.value < 0.05, TRUE, FALSE),
         sig2  = if_else(p.value < 0.001, TRUE, FALSE))


# Position text to report p value and R2
lm_text <- all |>
  group_by(trt_label) |>
  summarize(x = max(RWC_ind),
            y = 1) |>
  ungroup() |> 
  mutate(x = max(x)) |> 
  left_join(select(lm_params, 
                   trt_label, adj.r.squared, p.value, sig2)) |>
  mutate(lab1 = if_else(sig2 == TRUE,
                        paste0("p < 0.001"),
                        paste0("p==", round(p.value, 2))),
         lab2 = if_else(sig2 == TRUE,  
                        paste0("R^2==", round(adj.r.squared, 2)),
                        ""))
         # x = case_when(index_name2 == "RWC[ind]" ~ x-0.035,
         #               index_name2 == "WBI" ~ x - 0.005,
         #               .default = x))
lm_text

# colors for figure
cols_gn <- brewer.pal(4, "Paired")
cols_div <- brewer.pal(7, "Spectral")
strip <- strip_themed(background_y = elem_list_rect(fill = cols_div[c(6,3)]))

#### Figure for supplement ####
figs4 <-
  all |> 
  mutate(period = factor(period, levels = c("predawn", "midday"))) |> 
  ggplot(aes(x = RWC_ind, y = RWC)) +
  geom_point(aes(color = period)) +
  geom_abline(data = lm_params,
              aes(slope = -slope,
                  intercept = intercept,
                  linetype = sig)) +
  geom_text(data = lm_text,
            aes(x = x, y = y,
                label = lab1),
            parse = TRUE,
            vjust = 1,
            hjust = 0,
            size = 4) +
  geom_text(data = lm_text,
            aes(x = x, y = y-0.05,
                label = lab2),
            parse = TRUE,
            vjust = 1,
            hjust = 0,
            size = 4) +
  facet_wrap(~trt_label) +
  scale_x_reverse(expression(paste(RWC[ind]))) +
  scale_color_manual(values = cols_gn[4:3]) +
  scale_linetype_manual(values = c("solid")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.23, 0.1),
        legend.text = element_text(size = 12),
        legend.background = element_rect(fill = NA),
        strip.placement = "outside",
        strip.background.x = element_blank()) +
  guides(linetype = "none",
         color = guide_legend(keyheight = 0.03,
                              default.unit = "cm"))

ggsave(filename = "fig_scripts/round2/figS4.png",
       plot = figs4,
       height = 3.5,
       width = 8,
       units = "in")
