# Load in newly cleaned/averaged spectra
# And calcluate array of indices
library(tidyverse)

# Read in, pivot wider
spectra <- read_csv("data_clean/spectra_wp.csv") |> 
  pivot_wider(names_from = wavelength,
              values_from = refl_mean)

#### FUNCTIONS ####
#Calculates vegetation simple ratio (a:b) / (c:d)
calc_SR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  (rowMeans(df[b1_1:b1_2])/(rowMeans(df[b2_1:b2_2])))
}
#Calculates normalized index using (a:b - c:d) / (a:b + c:d)
calc_VI <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  ((rowMeans(df[b1_1:b1_2]) - rowMeans(df[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df[b1_1:b1_2]) + rowMeans(df[b2_1:b2_2])))
}
#Calculates difference ratio using (a:b - c:d) / (c:d)
calc_diffR <- function(df, a, b, c, d){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  ((rowMeans(df[b1_1:b1_2]) - rowMeans(df[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df[b2_1:b2_2])))
}
#Calculates additive ratio using (a:b + c:d) / (c:d + e:f)
calc_addR <- function(df, a, b, c, d, e, f){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  b3_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b3_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  ((rowMeans(df[b1_1:b1_2]) + rowMeans(df[b2_1:b2_2])) /      #vegetation index equation
      (rowMeans(df[b2_1:b2_2]) + rowMeans(df[b3_1:b3_2])))
}
#Calculates complex using (a:b) / (c:d + e:f + g:h + i:j)
calc_complex <- function(df, a, b, c, d, e, f, g, h, i, j){
  b1_1 <- which.min(abs(a - as.numeric(names(df), options(warn=-1))))   #Identify first band and start range
  b1_2 <- which.min(abs(b - as.numeric(names(df), options(warn=-1))))   #Identify first band and end range
  b2_1 <- which.min(abs(c - as.numeric(names(df), options(warn=-1))))   #Identify second band and start range
  b2_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify second band and end range
  b3_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b3_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify third band and end range
  b4_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fourth band and end range
  b4_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fourth band and end range
  b5_1 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fifth band and end range
  b5_2 <- which.min(abs(d - as.numeric(names(df), options(warn=-1))))   #Identify fifth band and end range
  ((rowMeans(df[b1_1:b1_2])) /      #vegetation index equation
      (rowMeans(df[b2_1:b2_2]) + rowMeans(df[b3_1:b3_2]) + 
         rowMeans(df[b4_1:b4_2]) + rowMeans(df[b5_1:b5_2])))
}

#### Calculate indices, then remove raw spectra averages ####
# Add in rwc
rwc <- read_csv("data_clean/wp_rwc_long.csv") |> 
  filter(variable == "RWC")

spectra_ind <- spectra |> 
  mutate(PRI = calc_VI(spectra, 531, 531, 570, 570), #PRI = (531-570)/(531+570)
         RWC_ind = calc_SR(spectra, 1430, 1430, 1850, 1850), # RWCind = 1430/1850
         WBI = calc_SR(spectra, 970, 970, 900, 900), # WBI = 970/900
         WPI1 = calc_diffR(spectra, 660, 670, 710, 720), # WPI1 = (664:670 - 710:720)/710:720,
         WPI2 = calc_addR(spectra, 660, 670, 710, 720, 1457, 1457) # WPI2 = (664:670 - 710:720)/(710:720 + 1457)
  ) |> 
  select(-`350`:-`2500`) |> 
  rename(WP = value) |> 
  select(-variable) |> 
  left_join(rwc) |> 
  select(-variable) |> 
  rename(RWC = value) |> 
  relocate(RWC, .after = WP)
        
write_csv(spectra_ind, "data_clean/spectra_ind_wp_rwc.csv")
