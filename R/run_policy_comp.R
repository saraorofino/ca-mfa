#' @title Build Your Own Policy Analysis 
#' @description
#' Pulls in reactive settings from Shiny users to run the model under a customized build your own policy consisting of target rates for source reduction, recycling, and post consumer recycled content. 
#' @return Returns a list of data frames and summary outputs for consumption, greenhouse gases and disposal outcomes cumulatively from implementation year.



# params ------------------------------------------------------------------
#delete in shiny


ca_rr_pack <- read.csv(here('data','static','ca_rr_pack.csv'))

library(tibble)

#sr
policy_rate_sr       <- 0.25
implement_year_sr <- 2024 #this could change
target_year_sr    <- 2032 #this could change
baseline_year_sr  <- 2023
target_sector_sr  <- 'pack'

#rr
policy_rate_rr <- 0.65
implement_year_rr <- 2024
target_year_rr <- 2032
target_sector_rr <- 'pack'

#rc
policy_rate_rc <- 0.4
implement_year_rc <- 2024
target_year_rc <- 2032
target_sector_rc <- 'pack' 



params_comp <- tibble(
  policy_rate_sr  = policy_rate,
  policy_rate_rr = policy_rate_rr,
  implement_year_54 = implement_year_comp,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)


run_policy_comp <- function(params_comp){
  
  # sr
  policy_rate_sr    <- params_comp$policy_rate_sr
  implement_year_sr <- params_comp$implement_year_sr
  target_year_sr    <- params_comp$target_year_sr
  baseline_year_sr  <- params_comp$baseline_year_sr
  target_sector_sr  <- params_comp$target_sector_sr
  
  # rr
  policy_rate_rr    <- params_comp$policy_rate_rr
  implement_year_rr <- params_comp$implement_year_rr
  target_year_rr    <- params_comp$target_year_rr
  target_sector_rr  <- params_comp$target_sector_rr
  
  # rc
  policy_rate_rc    <- params_comp$policy_rate_rc
  implement_year_rc <- params_comp$implement_year_rc
  target_year_rc    <- params_comp$target_year_rc
  target_sector_rc  <- params_comp$target_sector_rc
  baseline_rc  <- params_comp$baseline_rc
  is_scrap_consump  <- params_comp$is_scrap_consump
  

# Consumption -------------------------------------------------------------
# with recycled content
  
  
consum_comp <- calc_consum_sr(
                              consum_bau,
                              target_year_sr,
                              target_sr,
                              target_sector_sr,
                              baseline_year_sr,
                              implement_year_sr
                              )
  
rc_perc_comp <- calc_rc_perc(consum_comp,
                             target_rate_rc,
                             target_year_rc,
                             implement_year_rc,
                             target_sector_rc,
                             baseline_rc)  

scrap_input_comp <- calc_scrap_input(rc_perc_comp,
                                      is_scrap_consump)

avoid_virgin_comp <- calc_avoid_virgin(rc_perc_comp,
                                       is_scrap_consump)

  

# avoided primary production ----------------------------------------------
avoid_prod_comp <- calc_avoid_prod(consum_bau, consum_comp, summary = FALSE)

# Waste Generation  -------------------------------------------------------
 
wastegen_comp <- calc_wastegen(lifetimes, consum_comp)


# Waste Management --------------------------------------------------------

# collected recycling
collect_recyc_comp <- calc_collect_recyc(wastegen = wastegen_comp,
                                         bau_rr = ca_rr_pack,
                                         implement_year_rr = implement_year_rr,
                                         target_rr = policy_rate_rr,
                                         target_sector_rr = target_sector_rr,
                                         target_year_rr = target_year_rr)


# recycled output

recyc_output_comp <- calc_recyc_output(collect_recyc = collect_recyc_comp)
  

# end of life

eol_comp <- calc_eol(wastegen = wastegen_comp,
                     recyc_output = recyc_output_comp,
                     incineration = incineration)


# GHG ---------------------------------------------------------------------

ghg_comp <- calc_ghg(consum_comp,
                     emission_factors,
                     eol_comp,
                     target_sector_rc)

ghg_diff_comp <- calc_ghg_diff(
  ghg_prod = ghg_comp$ghg_prod,
  ghg_prod_bau = ghg_bau$ghg_prod,
  ghg_eol =ghg_comp$ghg_eol,
  ghg_eol_bau = ghg_bau$ghg_eol,
  ghg_avoid_prim_prod = ghg_comp$ghg_avoid_prim_prod,
  ghg_avoid_prim_prod_bau = ghg_bau$ghg_avoid_prim_prod,
  implement_year = implement_year_sr)

# Summary Output List ---------------------------------------------------------------

# using the lowest implement year for summaries?

implement_year_min <- min(implement_year_sr, implement_year_rr, implement_year_rc, na.rm = TRUE)

# consumption 

consum_comp_summary <- consum_comp |>
  filter(sector == 'all_sec') |>
  filter(year > implement_year_sr) 

total_consumption_comp <-  sum(consum_comp_summary$mt_plastic_sr)

#avoided primary production 

total_avoid_prod_comp <- calc_avoid_prod(consum_bau, consum_comp, summary = TRUE) 

# ghg summary

total_avoid_ghg_comp <- ghg_comp$ghg_avoid_prim_prod |>
  filter(year > implement_year) |>
  pull(mt_co2e_avoidprod) |>
  sum(na.rm = TRUE) * -1

#returning list of outputs

return(
  list(
    # values for policy comparison table
    total_consumption_comp = total_consumption_comp,
    total_avoid_prod_comp  = total_avoid_prod_comp,
    total_avoid_ghg_comp = total_avoid_ghg_comp,
    # data frames for graphing later
    consum_comp_data = consum_comp,
    eol_comp_data = eol_comp,
    ghg_comp_data = ghg_comp
  )
)

  
}



