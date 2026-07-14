#' @title 
#' @param 
#' @param 
#' @param 
#' @param 
#' @description
#' This function...
#' @return A data frame 'consum_total_byo" with columns for year, sector, and megatonnes of plastic 


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


#writing hard code

#creating a reduction multiplier, scaling linearly
consum_total_byo <- consum_bau_clean |> 
  #adding a column 'reduction factor' using linear scale based on inputs
  mutate(reduction_multiplier = ifelse(
    year <= target_year_sr,
    (1 - target_sr * 
       (year - baseline_year) / (target_year_sr - baseline_year)), #change to year1_rc
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