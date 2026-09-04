#' @title Run BAU
#' @description Inputs the EEIO total business as usual consumption based on state to policy to create summary outputs for policy comparison. 

run_bau <- function(consum_bau, incineration, emission_factors, lifetimes, bau_rr, target_sector)
  # add bau_rr to make dynamic to add national averages
{
 
  
  # Consumption Placeholder --------------------------------------------------
  

  #consum_bau_summary <- consum_bau |>  # add all_sec to data frame
  #  group_by(year) |>
  #  summarize(mt_plastic_bau = sum(mt_plastic_bau),
  #            .groups = "drop") |>
 #   mutate(sector = "all_sec")
  
 # consum_bau <- bind_rows(consum_bau, consum_bau_summary)
  
  
  # Waste Generation --------------------------------------------------------
  
  wastegen_bau <- calc_wastegen(lifetimes = lifetimes, consum = consum_bau) # Error message sectors not found
  
  
  # End of Life -------------------------------------------------------------
  

  
  eol_bau <- calc_eol_bau(wastegen = wastegen_bau, 
                          incineration = incineration,
                          bau_rr = bau_rr,
                          r_yield = 0.7,
                          target_sector = target_sector)
  
  # Greenhouse Gases --------------------------------------------------------
  ghg_bau <- calc_ghg(
    consum = consum_bau,
    eol = eol_bau,
    emission_factors = emission_factors,
    target_sector = 'pack',
    implement_year = 1950
  ) # hard code for business as usual to be start of model 
  
return(
  list(
    # data tables for policy functions 
    wastegen_bau = wastegen_bau,
    eol_bau = eol_bau,
    ghg_bau = ghg_bau))
  
}




