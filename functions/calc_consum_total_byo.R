# hard coding before I convert to function

library(tidyverse)
library(here)

consum_bau <- read_csv(here("data","consum-bau.csv"))
user_inputs_sb54 <- read_csv(here("data","user-inputs-sb54.csv"))

# wide format, need to extract values for user inputs first

target_year_sr <- user_inputs_sb54 |> 
  filter(name == "target_year_sr") |> 
  pull(value)

target_sr <- user_inputs_sb54 |> 
  filter(name == "target_sr") |> 
  pull(value)

baseline_year <- user_inputs_sb54 |> 
  filter(name == "baseline_year") |> 
  pull(value)

#creating a reduction multiplier, scaling linearly
reduction_multiplier <- ifelse(
  consum_bau$year <= target_year_sr,
  1- target_sr * (consum_bau$year - baseline_year) / (target_year_sr - baseline_year),
  1 - target_sr)

consum_total_byo <- consum_bau * reduction_multiplier


#wide form
reduction_multiplier <- ifelse(
  consum_bau$year <= user_inputs_sb54$target_year_sr,
        1- user_inputs_sb54$target_sr * (consum_bau$year - user_inputs_sb54$baseline_year) / (user_inputs_sb54$target_year - user_inputs_sb54$start_year),
                               1 - user_inputs_sb54$target_sr)
