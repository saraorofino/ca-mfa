#' @title Secondary Plastic Input for Recycled Content Targets
#' @param rc_perc_byo Data frame output of secondary plastic consumption from the recycled content policy.
#' @param recyc_yield Material loss rate from gathering recycled plastic to finished recycled content product. 
#' @param ca_scrap_consump In-state percent of scrap consumption.  
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors. 
#' @description
#' This function calculates scrap input to the state using the post consumer recycled content production target consumption to work backwards with the recycling yield loss to find the total amount of recycled plastic input. 
#' @return If summary 'FALSE' a data frame 'scrap_input" with columns for year, sector, and megatons of scrap input. 
#' @return If summary 'TRUE', a data frame 'summarized' with 1950-2050 cumulative, in-state and out-of-state scrap input in megatons. 


calc_scrap_input_function <- function(rc_perc_byo,
                                      recyc_yield,
                                      ca_scrap_consump,
                                      summary = FALSE) {
  detailed <- rc_perc_byo  %>%
    mutate(scrap_input = mt_plastic_rc / recyc_yield)
  
  if (!summary) {
    return(detailed)
  }
  summarized <- detailed %>%
    filter(sector != "all_sec") %>% # removes all sector totals per year
    summarise(total_scrap = sum(scrap_input)) %>%
    mutate(
      scrap_is = (total_scrap * ca_scrap_consump),
      scrap_oos = total_scrap * (1 - ca_scrap_consump)
    )
}

  
  
  
 