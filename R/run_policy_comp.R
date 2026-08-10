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



params_byo <- tibble(
  policy_rate_sr  = policy_rate,
  policy_rate_rr = policy_rate_rr,
  implement_year_54 = implement_year_sb54,
  target_year    = target_year,
  baseline_year  = baseline_year,
  target_sector  = target_sector
)


run_policy_comp <- function(params_byo){
  
  

# consumption -------------------------------------------------------------

consum_byo <- calc_consum_sr(consum_bau,
                             target_year_sr = target_year
)  
  
  
  
  
  
}



