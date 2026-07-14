# hard coding before I convert to function


# load data  --------------------------------------------------------------

library(tidyverse)
library(here)

consum_bau <- read_csv(here("data","static","consum_bau.csv"))
user_inputs_sb54 <- read_csv(here("data","static", "user_inputs_sb54.csv"))

consum_bau_clean <-  pivot_longer(
  consum_bau,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_bau")

write.csv(consum_bau_clean, "data/static/consum_bau_clean.csv", row.names = FALSE) #need to alter file path

# extract user values -----------------------------------------------------

target_year_sr <- user_inputs_sb54 |> 
  filter(name == "target_year_sr") |> 
  pull(value)

target_sr <- user_inputs_sb54 |> 
  filter(name == "target_sr") |> 
  pull(value)

baseline_year <-  user_inputs_sb54 |> 
  filter(name == "baseline_year") |> 
  pull(value)

implement_year <-  user_inputs_sb54 |> 
  filter(name == "implement_year") |> 
  pull(value)



# hard code ---------------------------------------------------------------
#creating a reduction multiplier, scaling linearly
consum_total_byo <- consum_bau_clean |> 
  #adding a column 'reduction factor' using linear scale based on inputs
  mutate(reduction_multiplier = ifelse(
  year <= target_year_sr,
  (1 - target_sr * 
      (year - baseline_year) / (target_year_sr - baseline_year)), #change to year1_rc multipliers use year 1, not year 0 baseline
  (1-target_sr))) |>
  mutate(
    mt_plastic_byo = case_when(
      
      sector == "pack" & year >= implement_year ~ 
                                    (consum_bau_clean |> 
                                    filter(year == baseline_year, sector == "pack") |> 
                                    pull(mt_plastic_bau) * reduction_multiplier),
  
      TRUE ~ mt_plastic_bau
      )) |> 
  select(!c(mt_plastic_bau, reduction_multiplier))


# test function -----------------------------------------------------------

calc_byo_function_output_test <- calc_byo_consum_function(consum_bau_clean,
                                                          target_year_sr,
                                                          target_sr,
                                                          baseline_year,
                                                          implement_year) 
identical(calc_byo_function_output_test, consum_total_byo)

