library(readxl)
library(tidyverse)
library(janitor)

# Read in treatments 
trts <- read_excel("data/treatments.xlsx") |> 
  mutate(plot_alt = sprintf("%02d", Plot),
         hp = glue::glue("H{House}P{Plot}"),
         trt = glue::glue("W{Winter}S{Summer}")) |> 
  select(hp, trt)

# Load leaf temps
# Read in water potential
leaft <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                 sheet = "Leaf Temp")  |> 
  pivot_longer(cols = starts_with("leaf"),
               names_to = "leaf_name",
               values_to = "leaf_t_c") |> 
  mutate(leaf = str_extract(leaf_name, "[A,B]")) |> 
  select(-notes, -leaf_name)

leaft_A <- leaft |> 
  filter(leaf_name)

# Read in porometer data with separate header
gs_head <- read_xls("data/gs/SC-1 Porometer Data 23-Aug-2023.xls",
                    n_max = 1)
colnames(gs_head)[c(1,5:6)] <- gs_head[1, c(1,5:6)]

gs_temp <- read_xls("data/gs/SC-1 Porometer Data 16-Aug-2023.xls",
               skip = 1,
               col_types = c("date", "numeric", "numeric", "text", "text",
                             "numeric", "numeric", "numeric"))
colnames(gs_temp) <- colnames(gs_head)
gs <- gs_temp |> 
  janitor::clean_names() |> 
  filter(grepl("^H", sample_id)) |> 
  mutate(hp = str_extract(sample_id, "H[0-9]P[0-9]{1,2}"),
         dica = str_extract(sample_id, "D[0-9]{1}"),
         leaf = str_extract(sample_id, "[A,B]$"),
         date = lubridate::date(measurement_time),
         time_char = as.character(measurement_time) |> 
           str_extract("[0-9]{2}:[0-9]{2}:[0-9]{2}"),
         time = lubridate::hms(time_char)) |> 
  left_join(trts, join_by(hp)) |> 
  



# Quick plot
gs |> 
  ggplot(aes(x = measurement_time,
             y = conductance,
             color = sample_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~trt) +
  theme_bw() +
  guides(color = FALSE)
