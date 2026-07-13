#' @title Avoided virgin plastic production
#' @param consum_total_byo data frame output of consumption after source reduction policy
#' @param rc_perc_byo data frame output of consumption after recycled content policy 
#' @param ca_scrap_consump In-state percent of scrap consumption 
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total mt_plastic_avoided across all years and the amount avoided in-state vs. out-of-state. 
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year recycled content amount from the consumption levels 
#' @return A data frame with columns for year, sector, and metric tons of avoided virgin plastic called mt_plastic_virgin for detailed. A summary data frame if summary is TRUE, with the total, in-state and out-of-state avoided production based on both recycled content and source reduction policies. 
#'
avoid_virgin_function <- function(consum_bau,consum_total_byo, rc_perc_byo, ca_scrap_consump, summary = FALSE) {
  detailed <- consum_total_byo %>%
    left_join(rc_perc_byo, by = c("year", "sector")) %>%
    left_join(consum_bau, by = c("year", "sector")) %>%
    mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc,
           mt_avoid_virgin = mt_plastic_bau - mt_plastic_virgin) %>%
    select(year, sector, mt_plastic_virgin, mt_avoid_virgin) 
  
  if(!summary) {
    return(detailed)
  }
  
 summarized <- detailed %>%
   summarise(total = sum(mt_avoid_virgin)) %>%
   mutate(mt_avoid_virgin_is = total * ca_scrap_consump,
        mt_avoid_virgin_oos = total * (1 - ca_scrap_consump))
} 

