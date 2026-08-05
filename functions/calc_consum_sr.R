#' @title Total Consumption under the source reduction function.
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param target_year_sr The target year for the source reduction to take full effect. 
#' @param target_sr The target source reduction, as a proportion.
#' @param target_sector_sr The sector which will be the target of the source reduction policy.
#' @param baseline_year_sr The baseline year of consumption under business as usual. Used to pull consumption value for source reduction
#' @param implement_year_sr The year of implementation of the source reduction policy. Reduction multipliers will be used in the following year, and baseline consumption will be pulled from the year prior.
#' @description This function calculates the plastic consumption under source reduction in the build your own scenario (BYO). Uses linear scaling to forecast source reduction rates based on the target source reduction policy input, and multiplies by the consumption under the business as usual scenario.
#' @return A data frame 'consum_sr" with columns for year, sector, and mega tones of plastic (mt_plastic_sr). Also contains an “all_sec” row each year with the total consumption across all sectors for that year.


calc_consum_sr <- function(consum_bau, 
                           target_year_sr, 
                           target_sr, 
                           target_sector_sr,
                           baseline_year_sr,
                           implement_year_sr) {
  
  # calculating the baseline sector value -----------------------------------
  
  baseline_sec_value <- consum_bau |> 
    filter(year == (baseline_year_sr), sector == target_sector_sr) |> 
    pull(mt_plastic_bau) 
  
  
  # calculating the reduction multiplier ------------------------------------
  
  consum_sr <- consum_bau |> 
    mutate(
      reduction_multiplier = ifelse(
        year <= target_year_sr,
        (1 - target_sr * (year - implement_year_sr) / (target_year_sr - implement_year_sr)), 
        (1 - target_sr)
      )
    ) |> 
    
    # calculating per sector consumption -----------------------------------------------
  
  mutate(
    mt_plastic_sr = case_when(
      sector == target_sector_sr & year > implement_year_sr~ baseline_sec_value * reduction_multiplier,
      TRUE ~ mt_plastic_bau
    )
  ) |> 
    select(-mt_plastic_bau, -reduction_multiplier)
  
  
  # calculating yearly totals across all sectors ----------------------------
  
  # removing existing BAU all_sec rows to recalculate all_sec totals per policy
  filter(sector != "all_sec") |> 
   
  all_sec <- consum_sr |>  #calculating the totals per year to later bind with full dataframe
    group_by(year) |> 
    summarize(
      sector = "all_sec",
     mt_plastic_sr = sum(mt_plastic_sr),
      .groups = "drop")
  
  consum_sr <- bind_rows(consum_sr, all_sec) |> 
    arrange(desc(year), sector == "all_sec", sector)
  
  return(consum_sr)
  
}


