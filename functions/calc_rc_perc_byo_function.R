#' @title Recycled content percentage Build Your Own (BYO) function.
#' @param consum_byo data frame output of consumption after source reduction policy
#' @param target_rc The target recycled content rate.
#' @param target_year_rc The target year for the full recycled content rate to be in effect.
#' @param baseline_year The baseline year of consumption.
#' @param implement_year The year of implementation for the recycled content mandate. 
#' @param target_sector The sector which will be the target of the recycled content mandate.
#' @param baseline_rc The baseline recycled content rate, used for linear scaling.
#' @description This function calculates the rate and weight of post consumer recycled content (PCR) in California plastic consumption due to PCR mandates.
#' @return A data frame with columns for year, sector, recycled content rate (rc_rate), and recycled content used in consumption in metric megatons (mt_plastic_rc_byo). Does not include all 'all_sec' row, which is the total across all sectors per year.

calc_rc_perc_byo_function <- function(consum_byo, target_rc, target_year_rc, baseline_year, implement_year, target_sector, baseline_rc) {
  

# calculating recycled content rate ---------------------------------------

  year1_rc <- baseline_year + 1
  
  rc_perc_byo <- consum_byo |> 
    filter(!c(sector == "all_sec")) |>  #removing all_sec
    mutate( rc_rate = 
              case_when(
                year >= implement_year & sector == 'pack' & year <= target_year_rc ~
                  baseline_rc + (target_rc - baseline_rc) * (year - year1_rc) / (target_year_rc - year1_rc), 
                year >= implement_year & sector == 'pack' & year >= target_year_rc ~ target_rc,  
                TRUE ~ 0)) 


# calculating recycled content weight -------------------------------------

  rc_perc_byo <- rc_perc_byo |> 
    mutate(mt_plastic_rc = mt_plastic_byo * rc_rate) |> 
    select(!c(mt_plastic_byo)) 

}