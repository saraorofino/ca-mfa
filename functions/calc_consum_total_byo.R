
# reading in data and packages --------------------------------------------

library(tidyverse)
library(here)

consum_bau <- read_csv(here("data","static","consum_bau.csv"))
user_inputs_sb54 <- read_csv(here("data","static", "user_inputs_sb54.csv"))


# user inputs -------------------------------------------------------------


target_year_sr <- user_inputs_sb54 |> 
  filter(name == "target_year_sr") |> 
  pull(value) |> 
  as.numeric()

target_sr <- user_inputs_sb54 |> 
  filter(name == "target_sr") |> 
  pull(value) |> 
  as.numeric()

baseline_year <-  user_inputs_sb54 |> 
  filter(name == "baseline_year") |> 
  pull(value) |> 
  as.numeric()

implement_year <-  user_inputs_sb54 |> 
  filter(name == "implement_year") |> 
  pull(value) |> 
  as.numeric()

target_sector <-  user_inputs_sb54 |> 
  filter(name == "target_sector") |> 
  pull(value)


# cleaning consum_bau_clean -----------------------------------------------


consum_bau_clean <-  pivot_longer(
  consum_bau,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_bau")

write.csv(consum_bau_clean, "data/static/consum_bau_clean.csv", row.names = FALSE) #need to alter file path


# hard code ---------------------------------------------------------------
#creating a reduction multiplier, scaling linearly
consum_total_byo <- consum_bau_clean |>
  #adding a column 'reduction factor' using linear scale based on inputs
  mutate(reduction_multiplier = ifelse(
    year <= target_year_sr,
    (
      1 - target_sr *
        (year - baseline_year) / (target_year_sr - baseline_year)
    ),
    #change to year1_rc multipliers use year 1, not year 0 baseline
    (1 - target_sr)
  )) |>
  mutate(
    mt_plastic_byo = case_when(
      sector == target_sector & year >= implement_year ~
        (
          consum_bau_clean |>
            #multiply the BAU consumption from the implementation year by reduction multiplier
            filter(year == baseline_year, sector == target_sector) |> #change to match
            pull(mt_plastic_bau) * reduction_multiplier
        ),
      
      TRUE ~ mt_plastic_bau #otherwise default to bau values
    )
  ) |>
  select(!c(mt_plastic_bau, reduction_multiplier))





# calculating total across all sectors ------------------------------------

all_sec <- consum_total_byo |>  #calculating the totals per year to later bind with full dataframe
  group_by(year) |> 
  summarize(
    sector = "all_sec",
    mt_plastic_byo = sum(mt_plastic_byo),
    .groups = "drop")

consum_total_byo <- bind_rows(consum_total_byo, all_sec) |> 
  arrange(desc(year), sector == "all_sec", sector) #binding totals back 



# writing it as a csv -----------------------------------------------------

write.csv(consum_total_byo, "data/consum_total_byo.csv", row.names = FALSE)

# test function -----------------------------------------------------------

calc_byo_function_output_test <- calc_byo_consum_function(consum_bau_clean,
                                                          target_year_sr,
                                                          target_sr,
                                                          baseline_year,
                                                          target_sector,
                                                          implement_year) 

calc_byo_consum_function(
  consum_bau_clean, 2032, 0.25, "pack", 2024
)


  

