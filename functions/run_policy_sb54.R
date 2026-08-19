#' @title SB 54 Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under CA SB54 targets.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.




run_policy_sb54 <- function(params_sb54, bau_results, incineration, consum_bau){
  
  target_sr   <- 0.25
  target_rr <- 0.65
  implement_year <- params_sb54$implement_year_54 #assuming same implement year
  target_year    <- params_sb54$target_year #assuming same target year 
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
  
  
# Avoided Primary Production ----------------------------------------------
  avoid_prod_sb54 <- calc_avoid_prod(consum_bau, consum_sb54, summary = FALSE)
  
# Waste Generation  -------------------------------------------------------
  wastegen_sb54 <- calc_wastegen(lifetimes, consum_sb54)

# Waste Management --------------------------------------------------------

# collected recycling
 collect_recyc_sb54 <- calc_collect_recyc(wastegen = wastegen_sb54,
                                          bau_rr_sect = ca_rr,
                                          implement_year_rr = implement_year,
                                          target_rr = target_rr,
                                          target_sector_rr = target_sector,
                                          target_year_rr = target_year
                                          )
  
# recycled output

  recyc_output_sb54 <- calc_recyc_output(collect_recyc = collect_recyc_sb54)

# end of life

  eol_sb54 <- calc_eol(wastegen = wastegen_sb54,
                       recyc_output = recyc_output_sb54,
                       incineration = incineration
                       )
  

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
  
  total_avoid_prod_sb54 <- calc_avoid_prod(consum_bau, consum_sb54, summary = TRUE) 
  
  # ghg summary
  
  total_avoid_ghg_sb54 <- ghg_sb54$ghg_avoid_prim_prod |>
    filter(year > implement_year) |>
    pull(mt_co2e_avoidprod) |>
    sum(na.rm = TRUE) * -1
  
  # Avoided GHG Compared to BAU 
  total_ghg_diff_sb54 <-  ghg_diff_sb54$total_diff
  
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
