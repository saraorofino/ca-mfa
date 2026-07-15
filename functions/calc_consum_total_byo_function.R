#' @title Total Consumption under the Build Your Own scenario function.
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param target_year_sr The target year for the source reduction to take full effect 
#' @param target_sr The target source reduction, as a proportion.
#' @param baseline_year The baseline year of consumption under business as usual. Used to pull consumption value for source reduction.
#' @param target_sector The sector which will be the target of the source reduction policy.
#' @param implement_year The year of implementation of the source reduction policy, where reduction multipliers will be used.
#' @description This function calculates the plastic consumption under source reduction in the build your own scenario (BYO). Uses linear scaling to forecast source reduction rates based on the target source reduction policy input, and multiplies by the consumption under the business as usual scenario.
#' @return A data frame 'consum_byo" with columns for year, sector, and mega tones of plastic (mt_plastic_byo). Also contains an “all_sec” row each year with the total consumption across all sectors for that year.


calc_byo_consum_function <- function(consum_bau, 
                                      target_year_sr, 
                                      target_sr, 
                                      baseline_year,
                                      target_sector,
                                      implement_year) {

# calculating the baseline sector value and year1_sr -----------------------------------
   
  baseline_sec_value <- consum_bau_clean |> 
    filter(year == baseline_year, sector == target_sector) |> 
    pull(mt_plastic_bau) 
  
  year1_sr <- baseline_year + 1
 

# calculating the reduction multiplier ------------------------------------
   
  consum_byo <- consum_bau |> 
    mutate(
      reduction_multiplier = ifelse(
        year <= target_year_sr,
        (1 - target_sr * (year - year1_sr) / (target_year_sr - year1_sr)), 
        (1 - target_sr)
      )
    )

# calculating per sector consumption -----------------------------------------------
  
  consum_byo <- consum_byo |>  
    mutate(
      mt_plastic_byo = case_when(
        sector == target_sector & year >= implement_year ~ baseline_sec_value * reduction_multiplier,
        TRUE ~ mt_plastic_bau
      )
    ) |> 
    select(-mt_plastic_bau, -reduction_multiplier)
  

# calculating yearly totals across all sectors ----------------------------

  all_sec <- consum_byo |>  #calculating the totals per year to later bind with full dataframe
    group_by(year) |> 
    summarize(
      sector = "all_sec",
      mt_plastic_byo = sum(mt_plastic_byo),
      .groups = "drop")
  
  consum_byo <- bind_rows(consum_total_byo, all_sec) |> 
    arrange(desc(year), sector == "all_sec", sector)



}


