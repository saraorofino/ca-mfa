
# reading in data and packages --------------------------------------------



library(tidyverse)
library(here)

consum_bau <- read_csv(here("data","consum-bau.csv"))
user_inputs_sb54 <- read_csv(here("data","user-inputs-sb54.csv"))


# cleaning cansum_bau_clean -----------------------------------------------


consum_bau_clean <-  pivot_longer(
  consum_bau,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_bau")

write.csv(consum_bau_clean, "data/consum_bau_clean.csv", row.names = FALSE) #need to alter file path


# extracting values from user inputs --------------------------------------


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



# hardcode ----------------------------------------------------------------


consum_total_byo <- consum_bau_clean |> 
  #adding a column 'reduction factor' using linear scale based on inputs
  mutate(reduction_multiplier = ifelse(
  year <= target_year_sr,
  (1 - target_sr * 
      (year - baseline_year) / (target_year_sr - baseline_year)),
  (1-target_sr))) |>
  mutate(
    mt_plastic_byo = case_when(
      # if the year is above the implementation year, multiply BAU consumption from the implementation year by reduction multiplier
      sector == "pack" & year >= implement_year ~ 
                                    (consum_bau_clean |> 
                                    filter(year == baseline_year, sector == "pack") |> 
                                    pull(mt_plastic_bau) * reduction_multiplier),
  
      TRUE ~ mt_plastic_bau
      )) |> 
  select(!c(mt_plastic_bau, reduction_multiplier))


# calculating totals ------------------------------------------------------

totals <- consum_total_byo |>  #calculating the totals per year to later bind with full dataframe
  group_by(year) |> 
  summarize(
    sector = "total",
    mt_plastic_byo = sum(mt_plastic_byo),
    .groups = "drop")


# adding totals to df -----------------------------------------------------

consum_total_byo <- bind_rows(consum_total_byo, totals) |> 
  arrange(desc(year), sector == "total", sector)

write.csv(consum_total_byo, "data/consum_total_byo.csv", row.names = FALSE)

  
