#' @title 
#' @param 
#' @param 
#' @param 
#' @param 
#' @description
#' This function...
#' @return A data frame 'consum_total_byo" with columns for year, sector, and megatonnes of plastic 

# can have the user inputs save into a data frame instead of pulling values 


#
calc_byo_consum_function <- function(consum_bau_clean, 
                                      target_year_sr, 
                                      target_sr, 
                                      baseline_year, 
                                      implement_year) {
  
  baseline_sec_value <- consum_bau_clean |> 
    filter(year == baseline_year, sector == "pack") |> 
    pull(mt_plastic_bau)
  
  consum_bau_clean |> 
    mutate(
      reduction_multiplier = ifelse(
        year <= target_year_sr,
        (1 - target_sr * (year - baseline_year) / (target_year_sr - baseline_year)), #change to year1_rc
        (1 - target_sr)
      )
    ) |> 
    mutate(
      mt_plastic_byo = case_when(
        sector == "pack" & year >= implement_year ~ baseline_sec_value * reduction_multiplier,
        TRUE ~ mt_plastic_bau
      )
    ) |> 
    select(-mt_plastic_bau, -reduction_multiplier)
    }