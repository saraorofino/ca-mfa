#' @title Avoided Plastic Production From Source Reduction
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param consum_sr Data frame output of consumption after source reduction policy.
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors.
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year consumption amount from source reduction from the total business as usual consumption levels.
#' @return If summary 'FALSE' a data frame 'avoid_prod' with columns for year, sector and avoided plastic production in megatons. 
#' @return If summary 'TRUE' a value 'avoid_prod_total' of 1950-2050 cumulative megatons of avoided plastic production across all sectors.  

calc_avoid_prod<- function(consum_bau, 
                           consum_scenario, 
                           recyc_output_bau, 
                           recyc_output_scenario,
                           displacement_rate = 0.8,
                           rc_perc = NULL, #default is null for RC perc, only when it is in RC and combined functions
                           is_scrap_consump = NULL,
                           summary = FALSE) {
  
  ## step 1: avoided production purely from reduced consumption / sr
  
  #RR, and RC use consum_bau DF with column name "mt_plastic_bau", renaming to "mt_plastic_sr" to be compatible below
  if ("mt_plastic_bau" %in% names(consum_scenario)) { 
    consum_scenario <- consum_scenario |>
      rename(mt_plastic_sr = mt_plastic_bau)
  }
  
  avoid_prod_consum <- consum_scenario |>
    left_join(consum_bau, by = c("year", "sector"), suffix = c("_scenario", "_bau")) |> #joining the 2 dataframes and adding suffix
    mutate(mt_avoid_prod_consum = mt_plastic_bau - mt_plastic_sr) |> #will return 0
    filter(sector != "all_sec") |> 
    select(year, sector, mt_avoid_prod_consum)
  
  
  ## step 2: displacement via recycling
  
 
  
  avoid_prod_recyc <- recyc_output_scenario |> 
    select(year, mt_secondary_plastic_output) |> #only selects secondary output (excludes landfill,etc)
    left_join(recyc_output_bau |> select(year, mt_secondary_plastic_output), 
              by = "year", suffix = c("_scenario", "_bau")) |> 
    mutate(mt_avoid_prod_recyc = (mt_secondary_plastic_output_scenario - mt_secondary_plastic_output_bau) * displacement_rate) |>
    select(year, mt_avoid_prod_recyc)
  

 ## combine avoided consum and avoided prod (via recycling)
  
  avoid_prod <- avoid_prod_consum |>
    left_join(avoid_prod_recyc, by = "year") |>
    mutate(mt_avoid_prod = mt_avoid_prod_consum + mt_avoid_prod_recyc) 
    
  ## step 3: displacement via PCR (only in RC scenarios, RC and combined)
  
  if (!is.null(rc_perc) && !is.null(is_scrap_consump)) {
    
    avoid_prod_rc <- rc_perc |>
      filter(sector != "all_sec") |>
      mutate(
        mt_avoid_virgin_is  = mt_plastic_rc * is_scrap_consump,
        mt_avoid_virgin_oos = mt_plastic_rc * (1 - is_scrap_consump),
        mt_avoid_prod_rc = mt_avoid_virgin_is + mt_avoid_virgin_oos
      ) |>
      select(year, sector, mt_avoid_prod_rc)
    
    avoid_prod <- avoid_prod |>
      left_join(avoid_prod_rc, by = c("year", "sector")) |>
      mutate(mt_avoid_prod = mt_avoid_prod + mt_avoid_prod_rc)
    
  }
  
  
  if (!summary) {
    return(avoid_prod)
  }
  
  avoid_prod_total <- avoid_prod |>
    filter(sector != "all_sec") |>
    summarise(
      total_consum = sum(mt_avoid_prod_consum, na.rm = TRUE),
      .by = NULL
    )
  
  # recycling component: one value per year, sum across years only (not sectors)
  total_recyc <- avoid_prod_recyc |>
    summarise(total_recyc = sum(mt_avoid_prod_recyc, na.rm = TRUE))
  
  avoid_prod_total <- tibble(total = avoid_prod_total$total_consum + total_recyc$total_recyc)
  
  
} 