#' @title Plastic Scrap Input to State
#' @param rc_perc_byo data frame output of consumption after recycled content policy
#' @param recyc_yield Material loss rate from gathering recycled plastic to finished recycled content product. 
#' @param ca_scrap_consump In-state percent of scrap consumption  
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total scrap across 1950 to 2050. 
#' @description
#' This code calculates scrap input to the state using the post consumer recycled content production target inputs to work backwards with the recycling yield loss to find the total amount of recycled plastic input. 
#' @return A data frame 'scrap_input" with columns for year, sector, and metric megatons of scrap input. 


# upload data -------------------------------------------------------------
# rc_perc_byo clean long format filler data frame for output, 0 in SB 54 
library(tidyr)
library(dplyr)

rc_perc_byo_clean <-pivot_longer(
  rc_perc_byo,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_rc"
)

write.csv(rc_perc_byo_54_clean, "data/static/rc_perc_byo_clean.csv", row.names = FALSE)


# hard code scrap input ----------------------------------------------------
recyc_yield <- 0.7 

scrap_input <- rc_perc_byo_clean %>%
  mutate(scrap_input = mt_plastic_rc / recyc_yield)

summarized <- scrap_input %>%
  filter(sector != "all_sec") %>% # removes all sector totals per year
  summarise(total_scrap = sum(scrap_input)) %>%
  mutate(scrap_is = total_scrap * )
  
