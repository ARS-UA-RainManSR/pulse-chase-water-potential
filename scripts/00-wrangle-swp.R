# compile SWP data

library(tidyverse)

# Read in each dat file, by house
# H3
temp <- read_csv("data/swp/CR1000_SWP_H3_20231013.dat",
                 skip = 1,
                 n_max = 1)

h3 <- read_csv("data/swp/CR1000_SWP_H3_20231013.dat",
               skip = 3)
colnames(h3) <- colnames(temp)

h3 <-  h3 |> 
  rename(H3P6_25cm_SWP = H3P6_25cm_SWC) # typo

# H1
# contains test plots outside of house, treat separately
temp <- read_csv("data/swp/CR1000_SWP_H1_20231013.dat",
                 skip = 1,
                 n_max = 1)

h1 <- read_csv("data/swp/CR1000_SWP_H1_20231013.dat",
               skip = 3)
colnames(h1) <- colnames(temp)

# H5
temp <- read_csv("data/swp/CR1000_H5_SWP_20231013.dat",
                 skip = 1,
                 n_max = 1)

h5 <- read_csv("data/swp/CR1000_H5_SWP_20231013.dat",
               skip = 3)
colnames(h5) <- colnames(temp)

#### Extract test sensors
test_25 <- h1 |> 
  select(TIMESTAMP, starts_with("test")) |> 
  rename(Test_25cm_SWP_teros = Test_25cm_SWP,
         Test_25cm_VWC_655 = Test_25cm_T) |> 
  pivot_longer(-TIMESTAMP,
               names_to = c("depth", "variable", "sensor"),
               names_pattern = "Test_(.*)_(.*)_(.*)",
               values_to = "value") 

#### Pivot longer and merge

swp_wide <- h3 |> 
  left_join(select(h1, !starts_with("Test")), by = join_by("TIMESTAMP")) |> 
  left_join(h5, by = join_by("TIMESTAMP")) |> 
  select(-starts_with("RECORD"))

swp_long <- swp_wide |> 
  pivot_longer(-TIMESTAMP,
               names_to = c("ID", "depth", "variable"),
               names_pattern = "(.*)_(.*)_(.*)",
               values_to = "value") |> 
  pivot_wider(names_from = variable,
              values_from = value)
         