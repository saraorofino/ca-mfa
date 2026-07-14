
# change 'total' to all_sec


library(tidyverse)
library(here)

consum_total_byo <- read_csv(here("data","consum_total_byo.csv"))
user_inputs_sb54 <- read_csv(here("data", "user-inputs-sb54.csv"))


# pulling values from user inputs -----------------------------------------

target_rc <- 0.4
  
  user_inputs_sb54 |> 
  filter(name == "target_rc") |> 
  pull(value)  |> 
  as.numeric()

target_year_rc <- user_inputs_sb54 |> 
  filter(name == "target_year_rc") |> 
  pull(value)  |> 
  as.numeric()

baseline_year <- user_inputs_sb54 |> 
  filter(name == "baseline_year") |> 
  pull(value) |> 
  as.numeric()

implement_year <- user_inputs_sb54 |> 
  filter(name == "implement_year") |> 
  pull(value) |> 
  as.numeric()

target_sector <- user_inputs_sb54 |> 
  filter(name == "target_sector") |> 
  pull(value)

baseline_rc <- 0 #should we add this to inputs


# calculating recycled content rate

consum_total_byo_rate_test <- consum_total_byo |> 
  filter(!c(sector == "total")) |>  #should we have another dataframe with just totals across all sectors?
  mutate( rc_rate = 
          case_when(
            year >= implement_year & sector == 'pack' & year <= target_year_rc ~
                     baseline_rc + (target_rc - baseline_rc) * (year - baseline_year) / (target_year_rc - baseline_year), 
          year >= implement_year & sector == 'pack' & year >= target_year_rc ~ target_rc,  
            
          TRUE ~ 0))

rc_perc_byo <-consum_total_byo_test |> 
  mutate(mt_plastic_byo_rc = mt_plastic_byo * rc_rate)
