#' @title SB 54 Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under CA SB54 targets.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.




run_policy_sb54 <- function(params, bau_results, incineration, consum_bau, bau_rr_sect, lifetimes, emission_factors){
  
  target_sr   <- 0.25
  target_rr <- 0.65
  implement_year <- params$implement_year_54 #assuming same implement year
  target_year    <- params$target_year #assuming same target year 
  baseline_year_sr  <- 2023
  target_sector <- 'pack' #assuming all have same target sector


# Consumption -------------------------------------------------------------

  consum_sb54 <- calc_consum_sr(
    consum_bau,
    target_year,
    target_sr,
    target_sector,
    baseline_year_sr,
    implement_year
  )
  
  

  
# Waste Generation  -------------------------------------------------------
  wastegen_sb54 <- calc_wastegen(lifetimes, consum_sb54)

# Waste Management --------------------------------------------------------



# end of life


  
  eol_sb54 <- calc_eol_policy(target_year = target_year, 
                              implement_year = implement_year,  
                              target_rr = target_rr,  
                              wastegen = wastegen_sb54, 
                              bau_eol = bau_results$eol_bau, 
                              incineration = incineration, 
                              r_yield = 0.7, 
                              target_sect = target_sector)
  

  
# ghg ---------------------------------------------------------------------



  ghg_sb54 <- calc_ghg(consum_sb54,
                     emission_factors,
                     eol_sb54,
                     target_sector,
                     implement_year)
  
  ghg_diff_sb54 <- calc_ghg_diff(
                            ghg_prod = ghg_sb54$ghg_prod,
                            ghg_prod_bau = bau_results$ghg_bau$ghg_prod,
                            ghg_eol =ghg_sb54$ghg_eol,
                            ghg_eol_bau = bau_results$ghg_bau$ghg_eol,
                            ghg_avoid_prim_prod = ghg_sb54$ghg_avoid_prim_prod,
                            ghg_avoid_prim_prod_bau = bau_results$ghg_bau$ghg_avoid_prim_prod,
                            implement_year = implement_year)

# Summary Output List ---------------------------------------------------------------

  # consumption 
  
  consum_sb54_summary <- consum_sb54 |>
    filter(sector == 'all_sec') |>
    filter(year > implement_year) 
  
  total_consumption_sb54 <-  sum(consum_sb54_summary$mt_plastic_sr)
  
  #avoided primary production 
  
  total_avoid_prod_sb54 <- calc_avoid_prod(consum_bau = consum_bau, 
                                           consum_scenario = consum_sb54, 
                                           eol_bau = bau_results$eol_bau, 
                                           eol_scenario = eol_sb54, 
                                           target_pcr= 0, 
                                           target_rr= target_rr, 
                                           rc_perc = NULL, 
                                           r_yield = 0.7, 
                                           displacement_rate = 0.8,
                                           is_scrap_consum = 0.5)
  
  
  # ghg summary
  
  total_avoid_ghg_sb54 <- ghg_sb54$ghg_avoid_prim_prod |>
    filter(year > implement_year) |>
    pull(mt_co2e_avoidprod) |>
    sum(na.rm = TRUE) * -1
  
  # Avoided GHG Compared to BAU 
  total_ghg_diff_sb54 <-  sum(ghg_diff_sb54$total_diff)
  
  #returning list of outputs
  
  return(
    list(
      # values for policy comparison table
      total_consumption_sb54 = total_consumption_sb54,
      total_avoid_prod_sb54  = total_avoid_prod_sb54,
      total_avoid_ghg_sb54 = total_avoid_ghg_sb54,
      total_ghg_diff_sb54 = total_ghg_diff_sb54, # Avoided Compared to BAU 
      # data frames for graphing later
      consum_sb54_data = consum_sb54,
      eol_sb54_data = eol_sb54,
      ghg_sb54_data = ghg_sb54,
      ghg_diff_sb54 = ghg_diff_sb54
    )
  )
  
  
}
