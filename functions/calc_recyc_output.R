#' @title Recycling Output 
#' @param collect_recyc Data frame output of the plastic collected based on recycling rate target.
#' @reference recycling yield reference 
#' @description
#' Calculates the output of secondary plastic from the state due to recycling rate changes using the recycling yield estimate of 70%. 

calc_recyc_output <- function(collect_recyc)
{
  recyc_output <- collect_recyc |>
    mutate(mt_secondary_plastic_output = mt_plastic_collect * 0.7) |>
    select(year, sector, mt_secondary_plastic_output)
  return(recyc_output)
}
  
