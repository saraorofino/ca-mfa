#' @title Calculating end of life measures under the chosen policy scenario
#' @param target_year the target year for the policy
#' @param implement_year the implementation year of the policy
#' @param target_rr the target recycling rate
#' @param wastegen dataframe output of calc_wastegen for the scenario
#' @param incineration static dataframe for incineration rates
#' @param r_yield hard coded assumption of a 0.7 recycling yield
#' @param target_sector the target sector to apply policy to
#' @description returns a dataframe with columns for weight of plastic landfilled, recycled and incinerated in million metric tons (Mt)




calc_eol_policy <- function(target_year, implement_year, target_rr, wastegen, bau_eol, incineration, r_yield = 0.7, target_sect){
  
  # comparison rate in 2050
  rate2050 <- bau_eol |> 
    filter(year == 2050) |> 
    pull(target_sect_recyc_rate)
  
  # pull baseline bau rate in implementation year 
  baseline_rate <- bau_eol |> 
    filter(year==implement_year) |> 
    pull(target_sect_recyc_rate)
  
  rr <- wastegen |> 
    filter(sector == target_sect) |> 
    left_join(bau_eol |> 
                dplyr::select(year, baseline_rr=target_sect_recyc_rate)) |> 
    mutate(rr_multiplier = case_when(
      target_rr <= rate2050 ~ baseline_rr,
      year <= implement_year ~ baseline_rr, 
      year > implement_year & year <= target_year ~ baseline_rate + (target_rr-baseline_rate) * (year-implement_year) / (target_year-implement_year),
      year >= target_year ~ target_rr
    )
    )
  
  # apply rr for target sector, summarize amt collected by year
  annual_amt_collected <- wastegen |> 
    left_join(rr |> 
                dplyr::select(sector, year, rr_multiplier), by = c('sector', 'year')) |> 
    mutate(mt_plastic_collected = ifelse(!is.na(rr_multiplier), mt_plastic_wastegen * rr_multiplier, 0)) |> 
    group_by(year) |> 
    summarize(mt_plastic_collected = sum(mt_plastic_collected), .groups="drop")
  
  # annual amts by fate all sectors
  annual_eol <- wastegen |> 
    filter(sector == "all_sec") |> 
    left_join(annual_amt_collected, by = "year") |> 
    left_join(incineration, by=c("year", "sector")) |> 
    mutate(mt_secondary_plastic_output = mt_plastic_collected * r_yield,
           mt_plastic_landfill = mt_plastic_wastegen-mt_secondary_plastic_output-mt_incin)
  
  return(annual_eol)
}