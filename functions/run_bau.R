#' @title Run BAU
#' @description Inputs the EEIO total business as usual consumption based on state to policy to create summary outputs for policy comparison. 

run_bau <- function(consum_bau, incineration, emission_factors, lifetimes, bau_rr_sect)
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
  
  #total recycling
  collect_recyc_bau <- calc_collect_recyc(wastegen = wastegen_bau,
                                          bau_rr_sect = bau_rr_sect, # change bau_rr_sect
                                          target_sector_rr = 'pack')
  
  recyc_output_bau <- calc_recyc_output(collect_recyc = collect_recyc_bau)
  
  # total end of life
  eol_bau <- calc_eol(
    wastegen = wastegen_bau,
    recyc_output = recyc_output_bau,
    incineration = incineration
  )
  
  
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
    collect_recyc_bau = collect_recyc_bau,
    recyc_output_bau = recyc_output_bau,
    eol_bau = eol_bau,
    ghg_bau = ghg_bau))
  
}




