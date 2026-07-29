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
wasteman_sr <- calc_eol(wastegen_sr, incineration, )
  
# Greenhouse Gas Emissions ------------------------------------------------
calc_ghg(consum_sr, emission_factors, )

# Summary Outputs List ---------------------------------------------------------

consum_sr_summary <- consum_sr |> 
    filter(sector == 'all_sec') |>
    filter(year >= implement_year_sr) # Totals only for implement year on 
  
total_consumption_sr <-  sum(consum_sr_summary$mt_plastic_sr)

return(list(
 ctotal_consumption_sr  = total_consumption_sr,
  scalar_mean  = mean_value,
  table_summary = summary_df,
  consum_sr_data   = consum_sr
))

}