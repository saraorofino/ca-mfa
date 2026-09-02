
  
  #' @title Source Reduction Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a source reduction target.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.




# Function for SR  --------------------------------------------------------
run_policy_sr <- function(params, bau_results, incineration, consum_bau, bau_rr_sect, lifetimes, emission_factors) {
  # pull in reactive inputs names will likely need to change
  target_sr   <- params$policy_rate
  implement_year_sr <- params$implement_year
  target_year_sr    <- params$target_year
  baseline_year_sr  <- params$baseline_year
  target_sector_sr <- params$target_sector
  
  # Consumption -------------------------------------------------------------
  consum_sr <- calc_consum_sr(
    consum_bau,
    target_year_sr,
    target_sr,
    target_sector_sr,
    baseline_year_sr,
    implement_year_sr
  )
  
  
  
  # Waste Generation  -------------------------------------------------------
  wastegen_sr <- calc_wastegen(lifetimes, consum_sr)
  
  # Waste Management  ------------------------------------------------------------
 
  
  eol_sr <- calc_eol_policy(target_year = target_year_sr, 
                  implement_year = implement_year_sr,  
                  target_rr = 0,  #will use BAU's recycling rate
                  wastegen = wastegen_sr, 
                  bau_eol = bau_results$eol_bau, 
                  incineration = incineration, 
                  r_yield = 0.7, 
                  target_sect = target_sector_sr)
  
  
  # Avoided Primary Production ----------------------------------------------
  avoid_prod_sr <- calc_avoid_prod(consum_bau = consum_bau, 
                                   consum_scenario = consum_sr, 
                                   recyc_output_bau = bau_results$eol_bau, 
                                   recyc_output_scenario = eol_sr,
                                   displacement_rate = 0.8,
                                   summary = FALSE)
  
  # Greenhouse Gas Emissions ------------------------------------------------
  ghg_sr <- calc_ghg(consum_sr,
                     emission_factors,
                     eol_sr,
                     target_sector_sr,
                     implement_year_sr)
  
  ghg_diff_sr <- calc_ghg_diff(
    ghg_prod = ghg_sr$ghg_prod,
    ghg_prod_bau = bau_results$ghg_bau$ghg_prod,
    ghg_eol = ghg_sr$ghg_eol,
    ghg_eol_bau = bau_results$ghg_bau$ghg_eol,
    ghg_avoid_prim_prod = ghg_sr$ghg_avoid_prim_prod,
    ghg_avoid_prim_prod_bau = bau_results$ghg_bau$ghg_avoid_prim_prod,
    implement_year = implement_year_sr
  )
  
  # Summary Outputs List ---------------------------------------------------------
  # Plastic Consumption
  consum_sr_summary <- consum_sr |>
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_sr) # Totals only for implement year on
  
  total_consumption_sr <-  sum(consum_sr_summary$mt_plastic_sr)
  
  # Avoided Primary Production
  total_avoid_prod_sr <- calc_avoid_prod(consum_bau = consum_bau, 
                                         consum_scenario = consum_sr, 
                                         recyc_output_bau = bau_results$eol_bau, 
                                         recyc_output_scenario = eol_sr,
                                         displacement_rate = 0.8,
                                         summary = TRUE) |>  pull(total_avoid_prod) # Calculates for 1950-2050 but avoided production only happens during implement years
  
  # Summary GHG
  
  total_avoid_ghg_sr <- sum(ghg_sr$ghg_avoid_prim_prod$mt_co2e_avoidprod) * -1
  
  # Total Avoided GHG Compared to BAU
  total_ghg_diff_sr <- sum(ghg_diff_sr$total_diff)
  
  #Total GHG emitted
  
  total_ghg_sr <- ghg_sr$ghg_total
  
  return(
    list(
      # values for policy comparison table
      total_consumption_sr = total_consumption_sr,
      total_avoid_prod_sr  = total_avoid_prod_sr,
      total_avoid_ghg_sr = total_avoid_ghg_sr,
      total_ghg_diff_sr = total_ghg_diff_sr, # compared to BAU
      total_ghg_sr = total_ghg_sr,
      # data frames for graphing later
      consum_sr_data = consum_sr,
      eol_sr_data = eol_sr,
      wastegen_sr_data = wastegen_sr,
      ghg_sr_data = ghg_sr,
      ghg_diff_sr = ghg_diff_sr
    )
  )
  
}

  
 