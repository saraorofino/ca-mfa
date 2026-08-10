#' @title Recycled Content Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a recycled content target. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year. 

# Params data frame placeholder for reactive inputs -----------------------
########  DELETE IN SHINY
library(tibble)

policy_rate       <- 0.40
implement_year <- 2025
target_year    <- 2032
target_sector  <- 'pack'

params <- tibble(
  policy_rate     = policy_rate,
  implement_year = implement_year,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)

run_policy_rc <- function(params) {
  # pull in reactive inputs names will likely need to change 
  target_rc   <-params$policy_rate
  implement_year_rc <-params$implement_year
  target_year_rc    <-params$target_year
  target_sector_rc <- params$target_sector
  
  # Consumption -------------------------------------------------------------
  # Consumption is not affected by recycled content rate
  consum_rc <- consum_bau
  
  # Avoided Primary Production ----------------------------------------------
  perc_rc <- calc_rc_perc(
    consum_rc,
    target_rc,
    target_year_rc,
    implement_year_rc,
    target_sector_rc,
    baseline_rc = 0
  ) # how much recycled content is produced

   avoid_prod_rc <-  calc_avoid_virgin(perc_rc, is_scrap_consump = 0.5) # hard code in assumption for all states 50% in state
  
  # Waste Generation  -------------------------------------------------------
  # Waste generation is not affected by recycling rate
  wastegen_rc <- calc_wastegen(lifetimes, consum_rc)
  
  # Waste Management  ------------------------------------------------------------
  
  collect_recyc_rc <- calc_collect_recyc(wastegen = wastegen_rc, bau_rr = ca_rr, target_sector_rr = target_sector_rc) 
  
  recyc_output_rc <- calc_recyc_output(collect_recyc_rc)
  
  eol_rc <- calc_eol(wastegen_rc, recyc_output_rc, incineration)
  
  # Greenhouse Gas Emissions ------------------------------------------------
  ghg_rc <- calc_ghg(consum_rr, emission_factors, eol_rc, target_sector_rc, implement_year_rc) 
  
  # Summary Outputs List ---------------------------------------------------------
  # Plastic Consumption 
  consum_rc_summary <- consum_rc |> 
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_rc) # Totals only for implement year on 
  
  total_consumption_rc <-  sum(consum_rc_summary$mt_plastic_rc)
  
  # Avoided Primary Production 
  total_avoid_prod_rc <- sum(avoid_prod_rc$total)
    
    return(
      list(
        # values for policy comparison
        total_consumption_rc = total_consumption_rc,
        total_avoid_prod_rc  = total_avoid_prod_rc,
        total_ghg_rc = total_ghg_rc,
        # data frames for graphing later
        consum_rr_data = consum_rc,
        wastegen_rr_data = wastegen_rc,
        eol_rc_data = eol_rc,
        ghg_rc_data = ghg_rc
      )
    )
  
}

