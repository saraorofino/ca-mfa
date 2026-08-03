#' @title SB 54 Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under CA SB54 targets.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.


# param inputs for SB54: delete in shiny ----------------------------------

library(tibble)

policy_rate_sr       <- 0.25
implement_year_sr <- 2025
target_year_sr    <- 2032
baseline_year_sr  <- 2023
target_sector_sr  <- 'pack'
policy_rate_rr <- 0.65


#should we assume same implementation and target year for rc, sr, rr etc?

params_sb54 <- tibble(
  policy_rate_sr  = policy_rate,
  policy_rate_rr = policy_rate_rr,
  implement_year = implement_year,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)

run_policy_sb54 <- function(params_sb54){
  
  target_sr   <- params_sb54$policy_rate_sr
  target_rr <- params_sb54$policy_rate_rr 
  implement_year <- params_sb54$implement_year #assuming same implement year
  target_year    <- params_sb54$target_year #assuming same target year 
  baseline_year_sr  <- params_sb54$baseline_year
  target_sector <- params_sb54$target_sector #assuming all have same target sector


# consumption -------------------------------------------------------------

  consum_sb54 <- calc_consum_sr(
    consum_bau,
    target_year,
    target_sr,
    target_sector,
    baseline_year_sr,
    implement_year
  )
  
  
  # Avoided Primary Production ----------------------------------------------
  avoid_prod_sb54 <- calc_avoid_prod(consum_bau, consum_sb54, summary = FALSE)
  
  # Waste Generation  -------------------------------------------------------
  wastegen_sr <- calc_wastegen(lifetimes, consum_sr)
  
}
