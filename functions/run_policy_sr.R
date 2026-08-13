#' @title Source Reduction Policy Analysis
#' @description
#' Pulls in reactive settings from Shiny users to run the model under stand alone policy of a source reduction target.
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.

# Params data frame placeholder for reactive inputs -----------------------
########  DELETE IN SHINY
library(tibble)

policy_rate       <- 0.25
implement_year <- 2025
target_year    <- 2032
baseline_year  <- 2023
target_sector  <- 'pack'

params <- tibble(
  policy_rate     = policy_rate,
  implement_year = implement_year,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)


# Function for SR  --------------------------------------------------------
### make run policy the same for all policies no _sr or _rr ?
run_policy_sr <- function(params) {
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
  
  # Avoided Primary Production ----------------------------------------------
  avoid_prod_sr <- calc_avoid_prod(consum_bau, consum_sr, summary = FALSE)
  
  # Waste Generation  -------------------------------------------------------
  wastegen_sr <- calc_wastegen(lifetimes, consum_sr)
  
  # Waste Management  ------------------------------------------------------------
  #Using SR waste generation and BAU recycle rates
  
  collect_recyc_sr <- calc_collect_recyc(wastegen = wastegen_sr,
                                         bau_rr_sect = ca_rr,
                                         target_sector_rr = target_sector_sr)
  # bau_rr could be reactive in future with national average, state by state recycling rates
  
  recyc_output_sr <- calc_recyc_output(collect_recyc_sr)
  
  eol_sr <- calc_eol(wastegen_sr, recyc_output_sr, ca_incineration)
  
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
  total_avoid_prod_sr <- calc_avoid_prod(consum_bau, consum_sr, summary = TRUE) # Calculates for 1950-2050 but avoided production only happens during implement years
  
  # Summary GHG
  
  total_avoid_ghg_sr <- sum(ghg_sr$ghg_avoid_prim_prod$mt_co2e_avoidprod) * -1
  
  return(
    list(
      # values for policy comparison table
      total_consumption_sr = total_consumption_sr,
      total_avoid_prod_sr  = total_avoid_prod_sr,
      total_avoid_ghg_sr = total_avoid_ghg_sr,
      # data frames for graphing later
      consum_sr_data = consum_sr,
      eol_sr_data = eol_sr,
      wastegen_sr_data = wastegen_sr,
      ghg_sr_data = ghg_sr,
      ghg_diff_sr = ghg_diff_sr
    )
  )
  
}
