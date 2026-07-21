#' @title Avoided Virgin Plastic Production
#' @param consum_sr Data frame output of consumption after source reduction policy.
#' @param rc_perc Data frame output of secondary plastic consumption from the recycled content policy. 
#' @param ca_scrap_consump In-state percent of scrap consumption. 
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with cumulative avoided primary plastic from 1950 to 2050 across all sectors.
#' @description
#' This function calculates the total virgin plastic consumed by taking the consumption after source reduction and subtracting the recycled content amounts per year by sector. 
#' @return If summary 'FALSE' a detailed data frame called 'avoid_virgin' with columns for year, sector, metric megatons of avoided virgin plastic 'mt_avoid_virgin' compared to business as usual and metric megatons of virgin plastic consumed after recycled content targets 'mt_plastic_virgin'. 
#' @return If summary 'TRUE', a summarized data frame 'avoid_virgin_total' with the total, in-state and out-of-state avoided production from 1950 to 2050 based on both recycled content and source reduction policies. 

calc_avoid_virgin <- function(consum_bau,consum_sr, rc_perc, is_scrap_consump, summary = FALSE) {
  avoid_virgin <- consum_sr |>
    left_join(rc_perc, by = c("year", "sector")) |>
    left_join(consum_bau, by = c("year", "sector")) |>
    mutate(mt_plastic_virgin = mt_plastic_sr - mt_plastic_rc,
           mt_avoid_virgin = mt_plastic_bau - mt_plastic_virgin) |>
    select(year, sector, mt_plastic_virgin, mt_avoid_virgin) 
  
  if(!summary) {
    return(avoid_virgin)
  }
  
 avoid_virgin_total <- avoid_virgin |>
   filter(sector != "all_sec") |> 
   summarise(total = sum(mt_avoid_virgin))|>
   mutate(mt_avoid_virgin_is = total * is_scrap_consump,
        mt_avoid_virgin_oos = total * (1 - is_scrap_consump)) |>
   return(avoid_virgin_total)
} 

