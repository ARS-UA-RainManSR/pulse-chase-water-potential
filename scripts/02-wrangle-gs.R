library(readxl)
library(googlesheets4)
library(tidyverse)
library(janitor)

# Read in treatments 
trts <- read_excel("data/treatments.xlsx") |> 
  mutate(plot_alt = sprintf("%02d", Plot),
         hp = glue::glue("H{House}P{Plot}"),
         trt = glue::glue("W{Winter}S{Summer}")) |> 
  select(hp, trt)

# Load leaf temps
leaft_init <- read_sheet("https://docs.google.com/spreadsheets/d/1R2auYuNOX0z3-01NkyFAtSn-aivZHvRNqWVb1_ThTcA/edit#gid=1859918857",
                 sheet = "Leaf Temp",
                 col_types = c("?nncnccnnnc")) |>
  rename(time_char = time)
str(leaft_init)

leaft <- leaft_init |> 
  pivot_longer(cols = starts_with("leaf"),
               names_to = "leaf_name",
               values_to = "leaf_t_c") |> 
  mutate(date_col = as.Date(date_col) |>
           force_tz(tzone = "America/Phoenix"),
         dt = as.POSIXct(paste0(date_col, " ", time_char), 
                         format = "%Y-%m-%d %H:%M"),
         leaf = str_extract(leaf_name, "[A,B]"),
         trt_s = str_extract(trt, "S[1-4]"),
         trt_w = str_extract(trt, "W[1-3]"),
         hp = paste0("H", house, "P", plot)) |> 
  select(-notes, -leaf_name) |>
  filter(leaf == "A") # Two leaves were attempted initially, but took too long

# Calculate start and end time for each round on each date
round_times <- leaft |>
  group_by(date_col, round) |>
  summarize(st = min(dt),
            en = max(dt)) |>
  mutate(en_4 = en + 4*60, # 4 mins after the last leaft
         en_530 = en + 5.5*60) # 5.5 mins after the last leaft

# Read in porometer data with separate header
gs_head <- read_xls("data/gs/SC-1 Porometer Data 23-Aug-2023.xls",
                    n_max = 1)
colnames(gs_head)[c(1,5:6)] <- gs_head[1, c(1,5:6)]

gs_temp1 <- read_xls("data/gs/SC-1 Porometer Data 23-Aug-2023.xls",
               skip = 1,
               col_types = c("date", "numeric", "numeric", "text", "text",
                             "numeric", "numeric", "numeric"))
gs_temp2 <- read_xls("data/gs/SC-1 Porometer Data 4-Sep-2023.xls",
                     skip = 1,
                     col_types = c("date", "numeric", "numeric", "text", "text",
                                   "numeric", "numeric", "numeric"))

colnames(gs_temp1) <- colnames(gs_head)
colnames(gs_temp2) <- colnames(gs_head)
gs <- gs_temp1 |> 
  bind_rows(gs_temp2) |>
  janitor::clean_names() |> 
  filter(grepl("^H", sample_id)) |> # eliminate calibration measurements, generally 1 measurement of each day
  rename(dt = measurement_time) |>
  mutate(dt = dt |>
           force_tz(tzone = "America/Phoenix"),
         hp = str_extract(sample_id, "H[0-9]P[0-9]{1,2}"),
         plant = str_extract(sample_id, "D[0-9]{1}"),
         leaf = str_extract(sample_id, "[A,B]$"),
         date = as.Date(dt)) |> 
  left_join(trts, join_by(hp)) |>
  filter(leaf == "A") |>
  left_join(round_times, by = join_by(date == date_col,
                                      dt >= st,
                                      dt <= en_530)) |>
  select(-st:-en_530)

# Both gs and leafT are a match

# join together
gst <- gs |>
  left_join(leaft, join_by(date == date_col, leaf, trt, round, plant, hp))

# write out
write_csv(gst, file = "data_clean/gs_leaftemp.csv")

gst <- read_csv(file = "data_clean/gs_leaftemp.csv")

# Get time ranges of rounds
gst |>
  group_by(round) |>
  summarize(st = min(time_char)/60/60,
            mean = mean(time_char)/60/60,
            en = max(time_char)/60/60)

# Quick plot timeseries of gs
gst |> 
  ggplot(aes(x = dt.x,
             y = conductance,
             color = hp)) +
  geom_point() +
  facet_wrap(~trt_s) +
  theme_bw() +
  guides(color = FALSE)

# delta T vs. conductance
gst |>
  mutate(deltaT = leaf_t_c - air_t_c) |>
  ggplot() +
  geom_point(aes(x = deltaT,
                 y = conductance,
                 color = trt_s)) +
  facet_wrap(~date)

# Summarize by date + round + trt_s
gs_round_trt <- gst |>
  group_by(date, round, trt_s) |>
  summarize(gs_m = mean(conductance),
            gs_sd = sd(conductance),
            gs_n = n())

gs_round_trt |>
  ggplot() +
  geom_errorbar(aes(x = date, 
                    ymin = gs_m - gs_sd,
                    ymax = gs_m + gs_sd,
                    color = factor(round)),
                width = 0) +
  geom_point(aes(x = date, y = gs_m, color = factor(round))) +
  geom_line(aes(x = date, y = gs_m, color = factor(round))) +
  facet_wrap(~trt_s, ncol = 3)

# Summarize by date + trt_s, lumping rounds
gs_trt <- gst |>
  group_by(date, trt_s) |>
  summarize(gs_m = mean(conductance),
            gs_sd = sd(conductance),
            gs_n = n())

gs_trt |>
  ggplot() +
  geom_errorbar(aes(x = date, 
                    ymin = gs_m - gs_sd,
                    ymax = gs_m + gs_sd),
                width = 0) +
  geom_point(aes(x = date, y = gs_m)) +
  geom_line(aes(x = date, y = gs_m)) +
  scale_y_continuous(expression(paste(g[s], " (mmol ", H[2], "O ", m^-2, " ", s^-1, ")"))) +
  facet_wrap(~trt_s, ncol = 3)
