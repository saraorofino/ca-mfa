#' @title Collected Recycling Business-as-Usual 
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @param policy Source reduction, Recycled Content, Recycle Rate, Combined or SB54
#' @reference PLACEHOLDER FOR CAL RECYCLE INFO
#' @description
#' PLACEHOLDER Description
#' 
#' 
#' 

policy = sr or policy = rc
bau_wastegen * bau_rr 

policy = rr 
bau_wastegen * rr_multiplier 


if (target_year_rr <= implement_year_rr) {
  stop("target_year must be after implement_year")
}



if (policy_type %in% c("sr", "rc")) {
  target_rr <- baseline_rate
}  



calc_collect_recyc <- function(policy, bau_rr) 
  
  wastegen |>
  mutate(
    baseline_year = implement_year_rr - 1,
    rr_multiplier = case_when(
      target_rr <= baseline_rate ~ bau_rr,
      year <= baseline_year ~ bau_rr, 
      year > baseline_year & year <= target_year_rr ~
        baseline_rate + (target_rr - baseline_rate) * (year - baseline_year) / (target_year_rr - baseline_year),
      year > target_year_rr ~ target_rr,
      TRUE ~ bau_rr
    )
  )  |>
  
  # calculating per year collection -----------------------------------------------

mutate(
  mt_plastic_collec = case_when(
    target_sector_rr == 'pack' & year > implement_year_rr ~ mt_plastic_wastegen * rr_multiplier, 
    TRUE ~ mt_plastic_wastegen * bau_rr
  )
) 


