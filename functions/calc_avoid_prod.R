#' @title Avoided Plastic Production From Source Reduction
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param consum_sr Data frame output of consumption after source reduction policy.
#' @param eol_bau Data frame output of end of life measures from business as usual (calc_eol_bau)
#' @param eol_scenario Data frame output of end of life measures from scenario (calc_eol_scenario)
#' @param target_pcr The target RC rate, used to determine which calculation steps to take
#' @param target_rr The target RR rate, used to determine which calculation steps to take
#' @param rc_perc Dataframe output from calc_rc_perc, contains rc rates. Defaults to NULL (only used in RC and combined)
#' @param r_yield Hard coded recycled yield of 0.7
#' @param displacement_rate hard coded dispalcement rate of 0.8
#' @param is_scrap_consum hard coded 0.5 rate of in state scrap consumption
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors.
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year consumption amount from source reduction from the total business as usual consumption levels.




calc_avoid_prod <- function(consum_bau, consum_scenario, eol_bau, eol_scenario, target_pcr=0, target_rr=0, rc_perc = NULL, r_yield = 0.7, displacement_rate = 0.8,
                            is_scrap_consum = 0.5){
  
  # change from source reduction direct effect
  
  #RR, and RC use consum_bau DF with column name "mt_plastic_bau", renaming to "mt_plastic_sr" to be compatible below
  if ("mt_plastic_bau" %in% names(consum_scenario)) { 
    consum_scenario <- consum_scenario |>
      rename(mt_plastic_sr = mt_plastic_bau)
  }
  
  delta_sr <- consum_bau |> 
    left_join(consum_scenario, by = c("year", "sector")) |>
    mutate(mt_diff = mt_plastic_bau - mt_plastic_sr) |>
    filter(sector == "all_sec") |> 
    group_by(sector) |> 
    summarize(mt_avoid_prod = sum(mt_diff), .groups="drop") |> 
    mutate(lever="source reduction")
  
  # change from recycled content
  if (target_pcr != 0) { 
    
    recyc_output <- consum_scenario |> 
      filter(sector != "all_sec") |> 
      left_join(rc_perc, by=c("year", "sector")) |>
      dplyr::mutate(mt_plastic_secondary = ifelse(!is.na(rc_rate), mt_plastic_sr*rc_rate, 0),
                    mt_plastic_scrap = mt_plastic_secondary/r_yield)
    
    delta_pcr_oos <- recyc_output |> 
      mutate(mt_virgin = mt_plastic_sr-mt_plastic_secondary) |> 
      group_by(year) |> 
      summarize(mt_consum = sum(mt_plastic_sr),
                mt_virgin = sum(mt_virgin), .groups="drop") |> 
      mutate(mt_avoid_prod = (mt_consum-mt_virgin) * (1-is_scrap_consum),
             sector="all_sec",
             lever="pcr_oos") |> 
      group_by(sector, lever) |> 
      summarize(mt_avoid_prod = sum(mt_avoid_prod), .groups="drop")
    
    plastic_collected_is <- recyc_output |> 
      group_by(year) |> 
      summarize(mt_plastic_is = sum(mt_plastic_scrap)*is_scrap_consum, .groups="drop") |> 
      left_join(eol_bau |> 
                  filter(sector=="all_sec") |> 
                  dplyr::select(sector, year, mt_plastic_collected_bau=mt_plastic_collected), by = "year") |> 
      mutate(mt_diff = pmax(mt_plastic_is - mt_plastic_collected_bau, 0))
    
  } else { 
    # ensures that if rc_perc = null (in sr, rr, sb54) the rest of teh function will continue
    delta_pcr_oos <- tibble(sector = "all_sec", lever = "pcr_oos", mt_avoid_prod = 0)
    plastic_collected_is <- eol_bau |> 
      filter(sector == "all_sec") |> 
      distinct(year, sector) |> 
      mutate(mt_diff = 0)
  }
  
  ### for pcr mandates only 
  if(target_rr == 0){
    
    delta_pcr_is <- plastic_collected_is |> 
      group_by(sector) |> 
      summarize(mt_diff_collected  = sum(mt_diff), .groups="drop") |> 
      mutate(mt_avoid_prod = mt_diff_collected * r_yield, # assumes 100% displacement for in state because pcr is a demand side intervention (pull policy, vs. push policy like rr is hoping for the best)
             lever="pcr_is") |> 
      dplyr::select(sector, mt_avoid_prod, lever)
  } else {
    
    # change due to rr 
    delta_rr <- eol_bau |> 
      rename(mt_plastic_collected_bau=mt_plastic_collected) |>
      left_join(eol_scenario, by = c("year", "sector")) |> 
      filter(sector=="all_sec") |> 
      # cfr-bau (column O in spreadsheet)
      mutate(mt_diff_rr = pmax(mt_plastic_collected - mt_plastic_collected_bau, 0)) 
    
    delta_pcr <- plastic_collected_is |> 
      dplyr::select(year, sector, mt_diff_pcr=mt_diff)
    
    delta_pcr_is <- delta_rr |> 
      left_join(delta_pcr, by = c("sector", "year")) |> 
      # pcr-rr (column R in spreadsheet)
      mutate(mt_diff_pcr_alone = pmax(mt_diff_pcr-mt_diff_rr, 0)) |> 
      group_by(sector) |> 
      summarize(mt_diff_collected  = sum(mt_diff_pcr_alone), .groups="drop") |> 
      mutate(mt_avoid_prod = mt_diff_collected * r_yield, # assumes 100% displacement for in state because pcr is a demand side intervention (pull policy, vs. push policy like rr is hoping for the best)
             lever="pcr_is") |> 
      dplyr::select(sector, mt_avoid_prod, lever)
    
  }
  
  # change from eol 
  ## source reduction only 
  if(target_pcr==0 & target_rr==0){
    delta_eol <- eol_bau |> 
      rename(mt_plastic_collected_bau=mt_plastic_collected) |> 
      left_join(eol_scenario, by = c("year", "sector")) |> 
      filter(sector=="all_sec") |> 
      group_by(sector) |>
      summarize(mt_bau = sum(mt_plastic_collected_bau),
                mt_policy = sum(mt_plastic_collected), .groups="drop") |>
      mutate(mt_diff_collected = mt_policy - mt_bau,
             mt_diff_recycled = mt_diff_collected * r_yield,
             mt_avoid_prod = mt_diff_recycled * displacement_rate,
             lever="eol recycling") |> 
      dplyr::select(sector, mt_avoid_prod, lever)
    
  } 
  ## recycling rate only 
  if(target_pcr==0 & target_rr!=0) {
    
    delta_eol <- eol_bau |> 
      dplyr::select(sector, year, mt_plastic_collected_bau=mt_plastic_collected) |>
      left_join(eol_scenario |> 
                  dplyr::select(year, sector, mt_plastic_collected), by = c("year", "sector")) |> 
      mutate(mt_diff = pmax(mt_plastic_collected - mt_plastic_collected_bau, 0)) |> 
      group_by(sector) |> 
      summarize(mt_diff_collected = sum(mt_diff), .groups="drop") |> 
      mutate(mt_diff_recycled = mt_diff_collected * r_yield,
             mt_avoid_prod = mt_diff_recycled * displacement_rate,
             lever="eol recycling") |> 
      dplyr::select(sector, mt_avoid_prod, lever)
    
  }
  ## pcr only is the out of state/in state calculation above
  if(target_pcr!=0 & target_rr==0){
    delta_eol <- data.frame(sector="all_sec",
                            mt_avoid_prod=0,
                            lever="eol recycling")
  }
  ## both rr and pcr
  if(target_pcr != 0 & target_rr != 0){
    
    # change due to rr 
    delta_rr <- eol_bau |> 
      rename(mt_plastic_collected_bau=mt_plastic_collected) |>
      left_join(eol_scenario, by = c("year", "sector")) |> 
      filter(sector=="all_sec") |> 
      # cfr-bau (column O in spreadsheet)
      mutate(mt_diff_rr = pmax(mt_plastic_collected - mt_plastic_collected_bau, 0)) 
    
    delta_pcr <- plastic_collected_is |> 
      dplyr::select(year, sector, mt_diff_pcr=mt_diff)
    
    # combine and compare
    delta_eol <- delta_rr |> 
      left_join(delta_pcr, by = c("year", "sector")) |> 
      ## column S: this is the maximum difference from BAU whether that is from rr or pcr [since rr is usually greater than pcr this is often equal to rr-bau]
      ## column O: difference between rr and bau [this is the same as the RR impact]
      ## column Q: rr-pcr amt to be removed because it could be caused by either policy 
      mutate(mt_diff_either = pmax(mt_diff_rr,mt_diff_pcr),
             mt_diff_rr = pmax(mt_diff_rr-mt_diff_pcr,0)) |> 
      group_by(sector) |> 
      summarize(mt_diff_collected_either = sum(mt_diff_either), 
                mt_diff_collected_rr = sum(mt_diff_rr), .groups="drop") |>
      mutate(mt_diff_collected_both = mt_diff_collected_either-mt_diff_collected_rr) |> 
      pivot_longer(cols = mt_diff_collected_either:mt_diff_collected_both,
                   names_to = "lever",
                   values_to = "mt_diff_collected") |> 
      # fix up lever names 
      mutate(lever = case_when(lever=="mt_diff_collected_either" ~ "pcr or eol recycling",
                               lever=="mt_diff_collected_rr" ~ "eol recycling",
                               lever=="mt_diff_collected_both" ~ "pcr and eol recycling together")) |> 
      # results don't need "either" just rr alone and pcr/rr together 
      filter(lever != "pcr or eol recycling") |> 
      # calculate avoided production 
      mutate(mt_diff_recycled = mt_diff_collected * r_yield,
             mt_avoid_prod = ifelse(lever=="eol recycling", mt_diff_recycled * displacement_rate, mt_diff_recycled*1)) |> # displacement should be 100% for the "both" 
      dplyr::select(sector, mt_avoid_prod, lever)
    
  }
  
  # Net impact
  net_impact <- delta_sr |> 
    bind_rows(delta_pcr_oos) |> 
    bind_rows(delta_pcr_is) |> 
    bind_rows(delta_eol)
  
  #total across all avoided production metrics
  
  total_avoid_prod <- net_impact |> 
    summarize(total_avoid_prod = sum(mt_avoid_prod, na.rm = TRUE)) |> 
    pull(total_avoid_prod)
  
  return(total_avoid_prod)
  
}