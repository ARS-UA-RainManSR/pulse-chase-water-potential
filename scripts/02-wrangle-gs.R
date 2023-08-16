library(readxl)
library(tidyverse)
library(janitor)

# Read in treatments to match
trts <- read_excel("data/treatments.xlsx") |> 
  mutate(plot_alt = sprintf("%02d", Plot),
         hp = glue::glue("H{House}P{plot_alt}"),
         trt = glue::glue("W{Winter}S{Summer}")) |> 
  select(hp, trt)

# Read in porometer data with separate header
gs_head <- read_xls("data/gs/SC-1 Porometer Data 3-Aug-2023_Hultine.xls",
                    n_max = 1)
colnames(gs_head)[c(1,5:6)] <- gs_head[1, c(1,5:6)]

gs_temp <- read_xls("data/gs/SC-1 Porometer Data 3-Aug-2023_Hultine.xls",
               skip = 1,
               col_types = c("date", "numeric", "numeric", "text", "text",
                             "numeric", "numeric", "numeric"))
colnames(gs_temp) <- colnames(gs_head)
gs <- gs_temp |> 
  janitor::clean_names() |> 
  filter(grepl("^H", sample_id),
         grepl("1$", sample_id)) |> 
  mutate(hp = str_extract(sample_id, "H[0-9]P[0-9]{2}")) |> 
  left_join(trts, join_by(hp))


# Quick plot
gs |> 
  ggplot(aes(x = measurement_time,
             y = conductance,
             color = sample_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~trt) +
  theme_bw()
