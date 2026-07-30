#' @title Source Reduction Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a source reduction target. 
#' @return Returns a list of dataframes and summary outputs for consumption, greenhouse gases and disposal outcomes cumulativley from implementation year. 

## copy over to combo policy 

run_policy <- function(params) {
  # pull in reactive inputs names will likely need to change 
  target_sr   <-0.25 #params$policy_rate
  implement_year_sr <-2025 #params$implement_year
  target_year_sr    <-2032 #params$target_year
  baseline_year_sr  <- 2023#params$baseline_year
  target_sector_sr <- 'pack' #params$target_sector

# Consumption -------------------------------------------------------------
consum_sr <-calc_consum_sr(
    consum_bau,
    target_year_sr,
    target_sr,
    target_sector_sr,
    baseline_year_sr,
    implement_year_sr
  )

# Avoided Primary Production ----------------------------------------------
avoid_prod_sr <- calc_avoid_prod(consum_bau, consum_sr,summary = FALSE)

# Waste Generation  -------------------------------------------------------
wastegen_sr <- calc_wastegen(lifetimes, consum_sr)

# Waste Management  ------------------------------------------------------------
#Using SR waste generation and BAU recycle rates
  
collect_recyc_sr <- calc_collect_recyc(wastegen = wastegen_sr, bau_rr = ca_rr, target_sector_rr = target_sector_sr) 
  # bau_rr could be reactive in future with national average, state by state recycling rates
  
recyc_output_sr <- calc_recyc_output(collect_recyc_sr)
  
wasteman_sr <- calc_eol(wastegen_sr, recyc_output_sr, incineration)
  
# Greenhouse Gas Emissions ------------------------------------------------
calc_ghg(consum_sr, emission_factors, )

# Summary Outputs List ---------------------------------------------------------
# Plastic Consumption 
consum_sr_summary <- consum_sr |> 
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_sr) # Totals only for implement year on 
  
total_consumption_sr <-  sum(consum_sr_summary$mt_plastic_sr)

# Avoided Primary Production 
avoid_prod_sr_summary <- calc_avoid_prod(consum_bau, consum_sr, summary = TRUE) |> 
  filter(sector == 'all_sec') |>
  filter(year >= implement_year_sr) # Totals only for implement year on 

total_avoid_prod_sr <- sum(avoid_prod_sr_summary)
return(
  list(
    # values for policy comparison
    total_consumption_sr = total_consumption_sr,
    total_avoid_prod_sr  = total_avoid_prod_sr,
    total_ghg_sr = total_ghg_sr,
    # data frames for graphing later
    consum_sr_data = consum_sr,
    wastegen_sr_data = wastegen_sr,
    wasteman_sr_data = wasteman_sr,
    ghg_sr_data = ghg_sr
  )
)

}