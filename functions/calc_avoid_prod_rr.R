#' @title Avoided Virgin Plastic Production from Recycle Rate 
#' @param recyc_output_rr Data frame output of secondary plastic from the recycling rate policy
#' @param recyc_output_bau Data frame output of secondary plastic from business as usual recycling rates
#' @param displacement_rate 0.8 e.g. 1kg of secondary plastic displaces 0.8kg virgin plastic. 
#' @description
#' This function summarizes the cumulative difference in avoided primary plastic from 1950 to 2050 across all sectors. 
#' @return A summarized data frame 'avoid_prod_total_rr' with the total, in-state and out-of-state avoided production from 1950 to 2050 based on recycling rate target. 


calc_avoid_prod_rr<- function(recyc_output_rr, recyc_output_bau, displacement_rate = 0.8, summary = FALSE) {
  avoid_prod_rr <- recyc_output_bau |>
    left_join(recyc_output_rr, by = c("year", "sector")) |>
    mutate(mt_avoid_prod = (mt_secondary_plastic_output_bau - mt_secondary_plastic_output_rr)* displacement_rate) |>
    filter(sector != "all_sec") |> 
    select(year, sector, mt_avoid_prod)
  
  if (!summary) {
    return(avoid_prod_rr)
  }
  
  avoid_prod_total_rr <- avoid_prod_rr |>
    filter(sector != "all_sec") |># removes all sector totals per year
    summarise(total = sum(mt_avoid_prod))
} 