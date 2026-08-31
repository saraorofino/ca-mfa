#' @title Avoided Plastic Production From Source Reduction
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param consum_sr Data frame output of consumption after source reduction policy.
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors.
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year consumption amount from source reduction from the total business as usual consumption levels.
#' @return If summary 'FALSE' a data frame 'avoid_prod' with columns for year, sector and avoided plastic production in megatons. 
#' @return If summary 'TRUE' a value 'avoid_prod_total' of 1950-2050 cumulative megatons of avoided plastic production across all sectors.  

calc_avoid_prod<- function(consum_bau, consum_sr, summary = FALSE) {
  
  ## step 1: avoided production purely from reduced consumption / sr
  
  avoid_prod_consum <- consum_scenario |>
    left_join(consum_bau, by = c("year", "sector"), suffix = c("_scenario", "_bau")) |> #joining the 2 dataframes and adding suffix
    mutate(mt_avoid_prod_consum = mt_plastic_bau - mt_plastic_sr) |> #will return 0
    filter(sector != "all_sec") |> 
    select(year, sector, mt_avoid_prod_consum)
  
  
  ## step 2: displacement via recycling
  
  avoid_prod_rr <- recyc_output_scenario |> 
    left_join(recyc_output_bau, by = c("year", "sector"), suffix = c("_scenario", "_bau")) |> 
    mutate(mt_avoid_prod_recyc = (mt_secondary_plastic_output_scenario - mt_secondary_plastic_output_bau) * displacement_rate) |>
    filter(sector != "all_sec") |>
    select(year, sector, mt_avoid_prod_recyc)
  
  ## step 3: combine them
  
  avoid_prod <- avoid_prod_consum |>
    left_join(avoid_prod_recyc, by = c("year", "sector")) |>
    mutate(mt_avoid_prod = mt_avoid_prod_consum + mt_avoid_prod_recyc) |>
    select(year, sector, mt_avoid_prod)
    
  
  
  
  
  if (!summary) {
    return(avoid_prod)
  }
  
  avoid_prod_total <- avoid_prod |>
    filter(sector != "all_sec") |># removes all sector totals per year
    summarise(total = sum(mt_avoid_prod))
  
  
  
  
} 