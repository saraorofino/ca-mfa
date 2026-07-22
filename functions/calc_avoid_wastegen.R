#' @title Calculating avoided waste generated between Business as Usual Scenario and Custom policy scenario.
#' @param wastegen_bau Data frame output of waste generated under the business as usual scenario
#' @param wastegen Data frame output of waste generated under the business as usual scenario
#' @summary Logical. If 'FALSE' (default), returns detailed data frame by year  If 'TRUE', returns a summary data frame with cumulative avoided waste generated from 1950 to 2050 across all sectors.
#' @return If summary 'FALSE' a data frame 'avoid_wastegen" with columns for year, sector, and mega tons of avoided waste generated.
#' @return If summary 'TRUE', a data frame 'avoid_wastegen_total' with 1950-2050 cumulative avoided waste generation.



calc_avoid_wastegen <- function(wastegen_bau, wastegen, summary = FALSE){
  
  wastegen_bau <- wastegen_bau |> 
    filter(sector == 'all_sec')
  
  wastegen <- wastegen |> 
    filter(sector == 'all_sec')
  
  avoid_wastegen <- wastegen_bau  |> 
    inner_join(
      wastegen,
      by = c("year", "sector"),
      suffix = c("_bau", "")) |> 
    
    mutate(mt_avoid_wastegen = (mt_plastic_wastegen_bau - mt_plastic_wastegen)) |> 
    select("year", "sector", "mt_avoid_wastegen")
  
  if(!summary){
    return(avoid_wastegen)
  }
  
  avoid_wastegen_total <- avoid_wastegen |> 
    summarise(total = sum(mt_avoid_wastegen, na.rm = TRUE))
  
  return(avoid_wastegen_total)
}