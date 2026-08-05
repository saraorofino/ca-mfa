#' @title SB 54 Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under CA SB54 targets.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.


# param inputs for SB54: delete in shiny ----------------------------------

#delete 
ca_rr_pack <- read.csv(here('data','static','ca_rr_pack.csv'))

library(tibble)

policy_rate_sr       <- 0.25
implement_year_54 <- 2024 #this could change
target_year_sr    <- 2032 #this could change
baseline_year_sr  <- 2023
target_sector_sr  <- 'pack'
policy_rate_rr <- 0.65


#should we assume same implementation and target year for rc, sr, rr etc?

params_sb54 <- tibble(
  policy_rate_sr  = policy_rate,
  policy_rate_rr = policy_rate_rr,
  implement_year_54 = implement_year_54,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)

run_policy_sb54 <- function(params_sb54){
  
  target_sr   <- params_sb54$policy_rate_sr
  target_rr <- params_sb54$policy_rate_rr 
  implement_year <- params_sb54$implement_year_54 #assuming same implement year
  target_year    <- params_sb54$target_year #assuming same target year 
  baseline_year_sr  <- params_sb54$baseline_year
  target_sector <- params_sb54$target_sector #assuming all have same target sector


# Consumption -------------------------------------------------------------

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
  wastegen_sb54 <- calc_wastegen(lifetimes, consum_sb54)

# Waste Management --------------------------------------------------------

# collected recycling
 collect_recyc_sb54 <- calc_collect_recyc(wastegen = wastegen_sb54,
                                          bau_rr = ca_rr_pack,
                                          implement_year_rr = 2024,
                                          target_rr = target_rr,
                                          target_sector_rr = target_sector,
                                          target_year_rr = target_year
                                          )
  
# recycled output

  recyc_output_sb54 <- calc_recyc_output(collect_recyc = collect_recyc_sb54)

# end of life

  eol_sb54 <- calc_eol(wastegen = wastegen_sb54,
                       recyc_output = recyc_output_sb54,
                       incineration =incineration
                       )
  
# GHG

  ghg_sb54 <- calc_ghg(consum_sb54,
                     emission_factors,
                     eol_sb54,
                     target_sector,
                     implement_year)
  

# Summary Output List ---------------------------------------------------------------

  # consumption 
  
  consum_sb54_summary <- consum_sb54 |>
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_sr) 
  
  total_consumption_sb54 <-  sum(consum_sb54_summary$mt_plastic_sr)
  
  #avoided primary production 
  
  total_avoid_prod_sb54 <- calc_avoid_prod(consum_bau, consum_sb54, summary = TRUE) 
  
  # ghg summary
  
  total_avoid_ghg_sb54 <- sum(ghg_sb54$ghg_avoid_prim_prod$mt_co2e_avoidprod) * -1
  
  #returning list of outputs
  
  return(
    list(
      # values for policy comparison table
      total_consumption_sb54 = total_consumption_sb54,
      total_avoid_prod_sb54  = total_avoid_prod_sb54,
      total_avoid_ghg_sb54 = total_avoid_ghg_sb54,
      # data frames for graphing later
      consum_sb54_data = consum_sb54,
      eol_sb54_data = eol_sb54,
      ghg_sb54_data = ghg_sb54
    )
  )
  
  
}
