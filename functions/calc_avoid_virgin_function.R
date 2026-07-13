#' @title Avoided virgin plastic production
#' @param consum_total_byo data frame output of consumption after source reduction policy
#' @param rc_perc_byo data frame output of consumption after
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total mt_plastic_avoided across all years and the amount avoided in-state vs. out-of-state. 
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year recycled content amount from the consumption levels 
#' @return A data frame with columns for year, sector, and metric tons of avoided virgin plastic called mt_plastic_virgin
#'
avoid_virgin_function <- function(consum_total_byo, rc_perc_byo, summary = FALSE) {
  detailed <- consum_total_byo %>%
    left_join(rc_perc_byo, by = c("year", "sector")) %>%
    mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
    select(year, sector, mt_plastic_virgin)
  if(!summary) {
    return(detailed)
  }
 total <- sum(detailed$mt_plastic_avoided)
   #####add in summary 
}

# Works 
avoid_virgin_function <- function(consum_total_byo, rc_perc_byo) {
  consum_total_byo %>%
    left_join(rc_perc_byo, by = c("year", "sector")) %>%
    mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
    select(year, sector, mt_plastic_virgin)
}