#' @title Recycling Rate Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a recycling rate target. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year. 

run_policy_rr <- function(params, bau_results, incineration, consum_bau, bau_rr_sect, lifetimes, emission_factors) {
  # pull in reactive inputs names will likely need to change
  target_rr          <- params$target_rr
  implement_year_rr  <- params$implement_year_rr
  target_year_rr     <- params$target_year_rr
  target_sector_rr   <- params$target_sector_rr
  
  # Consumption -------------------------------------------------------------
  # Consumption is not affected by recycling rate
  consum_rr <- consum_bau
  
  
  # Waste Generation  -------------------------------------------------------
  # Waste generation is not affected by recycling rate
  wastegen_rr <- calc_wastegen(lifetimes, consum_rr)
  
  # Waste Management  ------------------------------------------------------------
  
  
  
  eol_rr <- calc_eol_policy(target_year = target_year_rr, 
                            implement_year = implement_year_rr,  
                            target_rr = target_rr,  
                            wastegen = wastegen_rr, 
                            bau_eol = bau_results$eol_bau, 
                            incineration = incineration, 
                            r_yield = 0.7, 
                            target_sect = target_sector_rr)
  
  
  # Avoided Primary Production ----------------------------------------------
  # Assumed that recycled plastic at an 80% loss rate creates replacements for primary plastic

  
  avoid_prod_rr <- calc_avoid_prod(consum_bau = consum_bau, 
                                   consum_scenario = consum_rr, 
                                   recyc_output_bau = bau_results$eol_bau, 
                                   recyc_output_scenario = eol_rr,
                                   displacement_rate = 0.8,
                                   summary = FALSE
                                   )
  
  # Greenhouse Gas Emissions ------------------------------------------------
  ghg_rr <- calc_ghg(consum_rr,
                     emission_factors,
                     eol_rr,
                     target_sector_rr,
                     implement_year_rr)
  
  ghg_diff_rr <- calc_ghg_diff(
    ghg_prod = ghg_rr$ghg_prod,
    ghg_prod_bau = bau_results$ghg_bau$ghg_prod,
    ghg_eol = ghg_rr$ghg_eol,
    ghg_eol_bau = bau_results$ghg_bau$ghg_eol,
    ghg_avoid_prim_prod = ghg_rr$ghg_avoid_prim_prod,
    ghg_avoid_prim_prod_bau = bau_results$ghg_bau$ghg_avoid_prim_prod,
    implement_year = implement_year_rr
  )
  
  
  # Summary Outputs List ---------------------------------------------------------
  # Plastic Consumption Value
  consum_rr_summary <- consum_rr |>
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_rr) # Totals only for implement year on
  
  total_consumption_rr <-  sum(consum_rr_summary$mt_plastic_bau)
  
  # Avoided Primary Production Value
  total_avoid_prod_rr <- calc_avoid_prod(consum_bau = consum_bau, 
                                            consum_scenario = consum_rr, 
                                            recyc_output_bau = bau_results$eol_bau, 
                                            recyc_output_scenario = eol_rr,
                                            displacement_rate = 0.8,
                                            summary = TRUE
                                            ) |>  pull(total_avoid_prod)
  
  # Avoided GHG
  total_avoid_ghg_rr <- sum(ghg_rr$ghg_avoid_prim_prod$mt_co2e_avoidprod) * -1
  
  # Avoided GHG compared to BAU 
  
  total_ghg_diff_rr <- sum(ghg_diff_rr$total_diff)
  
# Total GHG implement year on 
  
  total_ghg_rr <- ghg_rr$ghg_total
  
  return(
    list(
      # values for policy comparison
      total_consumption_rr = total_consumption_rr,
      total_avoid_prod_rr  = total_avoid_prod_rr,
      total_avoid_ghg_rr = total_avoid_ghg_rr,
      total_ghg_diff_rr = total_ghg_diff_rr, # compared to BAU 
      total_ghg_rr = total_ghg_rr,
      
      # data frames for graphing later
      
      consum_rr_data = consum_rr,
      wastegen_rr_data = wastegen_rr,
      eol_rr_data = eol_rr,
      ghg_rr_data = ghg_rr,
      ghg_diff_rr = ghg_diff_rr
    )
  )
  
}
