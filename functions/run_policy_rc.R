#' @title Recycled Content Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a recycled content target. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year. 



run_policy_rc <- function(params, bau_results, incineration, consum_bau, bau_rr_sect, lifetimes, emission_factors) {
  # pull in reactive inputs names will likely need to change 
  target_rc          <- params$target_rc
  implement_year_rc  <- params$implement_year_rc
  target_year_rc     <- params$target_year_rc
  target_sector_rc   <- params$target_sector_rc
  
  # Consumption -------------------------------------------------------------
  # Consumption is not affected by recycled content rate
  consum_rc <- consum_bau
  
  # Avoided Primary Production ----------------------------------------------
  rc_perc <- calc_rc_perc(
    consum_rc,
    target_rc,
    target_year_rc,
    implement_year_rc,
    target_sector_rc,
    baseline_rc = 0
  ) # how much recycled content is produced

   
  
  # Waste Generation  -------------------------------------------------------
  # Waste generation is not affected by recycling rate
  wastegen_rc <- calc_wastegen(lifetimes, consum_rc)
  
  # Waste Management  ------------------------------------------------------------
  

  
  eol_rc <- calc_eol_policy(target_year = target_year_rc, 
                            implement_year = implement_year_rc,  
                            target_rr = 0,  #will use BAU's recycling rate
                            wastegen = wastegen_rc, 
                            bau_eol = bau_results$eol_bau, 
                            incineration = incineration, 
                            r_yield = 0.7, 
                            target_sect = target_sector_rc)
  
  #avoided production
  
  avoid_prod_rc <- calc_avoid_prod(consum_bau = consum_bau, 
                                   consum_scenario = consum_rc, 
                                   recyc_output_bau = bau_results$eol_bau, 
                                   recyc_output_scenario = eol_rc,
                                   displacement_rate = 0.8,
                                   rc_perc = rc_perc, 
                                   is_scrap_consump = 0.5, #hardcoded 0.5 instate scrap consum
                                   summary = TRUE)
  
  # Greenhouse Gas Emissions ------------------------------------------------
  ghg_rc <- calc_ghg(consum_rc, emission_factors, eol_rc, target_sector_rc, implement_year_rc) 
  
  ghg_diff_rc <- calc_ghg_diff(
    ghg_prod = ghg_rc$ghg_prod,
    ghg_prod_bau = bau_results$ghg_bau$ghg_prod,
    ghg_eol = ghg_rc$ghg_eol,
    ghg_eol_bau = bau_results$ghg_bau$ghg_eol,
    ghg_avoid_prim_prod = ghg_rc$ghg_avoid_prim_prod,
    ghg_avoid_prim_prod_bau = bau_results$ghg_bau$ghg_avoid_prim_prod,
    implement_year = implement_year_rc
  )
  
  # Summary Outputs List ---------------------------------------------------------
  # Plastic Consumption 
  consum_rc_summary <- consum_rc |> 
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_rc) # Totals only for implement year on 
  
  total_consumption_rc <-  sum(consum_rc_summary$mt_plastic_bau)
  
  # Avoided Primary Production 
  total_avoid_prod_rc <- calc_avoid_prod(consum_bau = consum_bau, 
                                   consum_scenario = consum_rc, 
                                   recyc_output_bau = bau_results$eol_bau, 
                                   recyc_output_scenario = eol_rc,
                                   displacement_rate = 0.8,
                                   rc_perc = rc_perc, 
                                   is_scrap_consump = 0.5, #hardcoded 0.5 instate scrap consum
                                   summary = TRUE) |>  pull(total_avoid_prod) #ensuring it is a single number for graphing
  
  # Total avoided ghg without BAU
  
  total_avoid_ghg_rc <- sum(ghg_rc$ghg_avoid_prim_prod$mt_co2e_avoidprod) * -1
  
  # Total avoided GHG compared to BAU
  total_ghg_diff_rc <- sum(ghg_diff_rc$total_diff)
  
  # total ghg
  total_ghg_rc <- ghg_rc$ghg_total
    
    return(
      list(
        # values for policy comparison
        total_consumption_rc = total_consumption_rc,
        total_avoid_prod_rc  = total_avoid_prod_rc,
        total_avoid_ghg_rc = total_avoid_ghg_rc,
        total_ghg_diff_rc = total_ghg_diff_rc, # compared to BAU
        total_ghg_rc = total_ghg_rc,
        # data frames for graphing later
        consum_rc_data = consum_rc,
        wastegen_rc_data = wastegen_rc,
        eol_rc_data = eol_rc,
        ghg_rc_data = ghg_rc,
        ghg_diff_rc = ghg_diff_rc
      )
    )
  
}

