#' @title 
#' @param 
#' @param 
#' @param 
#' @param 
#' @description
#' This function...
#' @return A data frame 'consum_total_byo" with columns for year, sector, and megatonnes of plastic 



calc_byo_consum_function <- function(consum_bau_clean, 
                                      target_year_sr, 
                                      target_sr, 
                                      baseline_year,
                                      target_sector,
                                      implement_year) {
  
  baseline_sec_value <- consum_bau_clean |> 
    filter(year == baseline_year, sector == target_sector) |> 
    pull(mt_plastic_bau) 
  
  year1_sr <- baseline_year + 1
  
  consum_byo_total <- consum_bau_clean |> 
    mutate(
      reduction_multiplier = ifelse(
        year <= target_year_sr,
        (1 - target_sr * (year - year1_sr) / (target_year_sr - year1_sr)), 
        (1 - target_sr)
      )
    ) |> 
    mutate(
      mt_plastic_byo = case_when(
        sector == target_sector & year >= implement_year ~ baseline_sec_value * reduction_multiplier,
        TRUE ~ mt_plastic_bau
      )
    ) |> 
    select(-mt_plastic_bau, -reduction_multiplier)
    }