#' @title Avoided Virgin Plastic Production from Recycled Content 
#' @param rc_perc Data frame output of secondary plastic consumption from the recycled content policy, either from source reduction or business-as-usual consumption depending on policy impact. 
#' @param is_scrap_consump In-state percent of scrap consumption. 
#' @description
#' This function summarizes the cumulative primary plastic from 1950 to 2050 across all sectors. For recycled content policy isolated impacts, change the consumption inputs to rc_perc. 
#' @return A summarized data frame 'avoid_virgin_total' with the total, in-state and out-of-state avoided production from 1950 to 2050 based on both recycled content policy. 

calc_avoid_virgin <- function(rc_perc,
                              is_scrap_consump) {

  avoid_virgin <- rc_perc |>
    filter(sector != "all_sec") |>
    summarise(total = sum(mt_plastic_rc)) |>
    mutate(
      mt_avoid_virgin_is = total * is_scrap_consump,
      mt_avoid_virgin_oos = total * (1 - is_scrap_consump)
    )
  return(avoid_virgin)
}
