#' @title Recycling Rate Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a recycling rate target. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year. 

run_policy <- function(params) {
  # pull in reactive inputs names will likely need to change 
  target_rr   <-params$policy_rate
  implement_year_rr <-params$implement_year
  target_year_rr    <-params$target_year
  baseline_year_rr  <- params$baseline_year # only for sr
  target_sector_rr <- params$target_sector
  
  # Consumption -------------------------------------------------------------
  # Consumption is not affected by recycled content rate
  consum_rr <- consum_bau
  
  # Avoided Primary Production ----------------------------------------------
  calc_rc_perc()
  calc_avoid_virgin(rc_perc_rr, is_scrap_consump = 0.5) # hard code in assumption for all states 50% in state
  
  # Waste Generation  -------------------------------------------------------
  # Waste generation is not affected by recycling rate
  wastegen_rr <- calc_wastegen(lifetimes, consum_rr) 
  
  # Waste Management  ------------------------------------------------------------
  
  collect_recyc_rr <- calc_collect_recyc(wastegen = wastegen_rr, bau_rr = ca_rr, target_sector_rr = target_sector_rr) 
  
  recyc_output_rr <- calc_recyc_output(collect_recyc_rr)
  
  eol_rr <- calc_eol(wastegen_rr, recyc_output_rr, incineration)
  
  # Greenhouse Gas Emissions ------------------------------------------------
  ghg_rr <- calc_ghg(consum_rr, emission_factors, wasteman_rr, target_sector_rr, implement_year_rr) # GHG function not working
  
  # Summary Outputs List ---------------------------------------------------------
  # Plastic Consumption 
  consum_rr_summary <- consum_rr |> 
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_rr) # Totals only for implement year on 
  
  total_consumption_rr <-  sum(consum_rr_summary$mt_plastic_rr)
  
  # Avoided Primary Production 
  total_avoid_prod_rr <- 
    
    return(
      list(
        # values for policy comparison
        total_consumption_rr = total_consumption_rr,
        total_avoid_prod_rr  = total_avoid_prod_rr,
        total_ghg_rr = total_ghg_rr,
        # data frames for graphing later
        consum_rr_data = consum_rr,
        wastegen_rr_data = wastegen_rr,
        eol_rr_data = eol_rr,
        ghg_rr_data = ghg_rr
      )
    )
  
}

# Avoided Primary Production ----------------------------------------------
calc_avoid_virgin(rc_perc_rr, is_scrap_consump = 0.5) # hard code in assumption for all states 50% in state
