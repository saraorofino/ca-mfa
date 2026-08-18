#' @title Calculate plastic sector proportions
#' @Description Calculate the average breakdown of consumption by plastic sector 2012-2020 
#' 
#' @param a_consum the power series approximation of plastic consumption each year; output
#' of the function calc_a_consum 
#' @param props the proportion of each BEA summary sector that falls into a given plastic
#' sector; output of the function preprocess_sectors
#'
#' @return the average proportion of consumption allocated to each of the 11 plastic sectors 

calc_plastic_sector_props <- function(a_consum, props) {

  a_consum_clean_mt <- a_consum |>
    dplyr::select(-total_mt) |> 
    pivot_longer(cols = "oem_mt":"tier_4_mt",
                 names_to = "variable",
                 values_to = "consum_mt") |> 
    mutate(bea_summary_clean = str_extract(row, "^[^/]+"))
  
  # join props and calculate average sector breakdown 2012-2020
  avg_props <- a_consum_clean_mt |> 
    left_join(props, by = c("bea_summary_clean"="bea_summary"), relationship = "many-to-many") |> 
    mutate(plastic_consum_mt = consum_mt * prop_consum) |> 
    # calculate annual total
    group_by(year) |> 
    mutate(annual_total_consum_mt = sum(plastic_consum_mt)) |> 
    ungroup() |> 
    # annual plastic sector totals
    group_by(year, annual_total_consum_mt, plastic_sector) |> 
    summarize(plastic_consum_mt = sum(plastic_consum_mt), .groups="drop") |> 
    # prop annual total for each plastic sector
    mutate(prop_annual_total = plastic_consum_mt / annual_total_consum_mt) |> 
    # avg proportion over timeseries 
    summarize(avg_prop = mean(prop_annual_total), .by="plastic_sector")
  
  return(avg_props)

}
