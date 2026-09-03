#' @title Recycled content percentage function.
#' @param consum data frame output of consumption after source reduction policy or BAU depending on policy interaction. 
#' @param target_rc The target recycled content rate.
#' @param target_year_rc The target year for the full recycled content rate to be in effect.
#' @param implement_year The year of implementation for the recycled content mandate. 
#' @param target_sector The sector which will be the target of the recycled content mandate.
#' @param baseline_rc The baseline recycled content rate, used for linear scaling.
#' @description This function calculates the rate and weight of post consumer recycled content (PCR) in California plastic consumption due to PCR mandates.
#' @return A data frame with columns for year, sector, recycled content rate (rc_rate), and recycled content used in consumption in metric megatons (mt_plastic_rc). Does not include all 'all_sec' row, which is the total across all sectors per year.

calc_rc_perc <- function(consum, target_rc, target_year_rc, implement_year, target_sector, baseline_rc) {
  

# calculating recycled content rate ---------------------------------------

  
  rc_perc <- consum |> 
    filter(!c(sector == "all_sec")) |>  #removing all_sec
    mutate( rc_rate = 
              case_when(
                year >= implement_year & sector == target_sector & year <= target_year_rc ~
                  baseline_rc + (target_rc - baseline_rc) * (year - implement_year) / (target_year_rc - implement_year), 
                year > implement_year & sector == target_sector & year >= target_year_rc ~ target_rc,  
                TRUE ~ 0)) 


# calculating recycled content weight -------------------------------------

  rc_perc <- rc_perc |> 
    mutate (mt_plastic_rc = pull(across(starts_with("mt_plastic")))* rc_rate) |> 
    select(year, sector, mt_plastic_rc, rc_rate)
  
  
  return(rc_perc)

}