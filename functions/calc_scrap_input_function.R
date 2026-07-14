#' @title Scrap plastic Needed for Recycled Content Targets
#' @param rc_perc_byo data frame output of consumption after recycled content policy
#' @param recyc_yield Material loss rate from gathering recycled plastic to finished recycled content product. 
#' @param ca_scrap_consump In-state percent of scrap consumption  
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector of total scrap plastic feedstock. If 'TRUE', returns a summary data frame with total scrap, in-state scrap and out-of-state scrap across 1950 to 2050. 
#' @description
#' This function calculates scrap input to the state using the post consumer recycled content production target inputs to work backwards with the recycling yield loss to find the total amount of recycled plastic input. 
#' @return A data frame 'scrap_input" with columns for year, sector, and megatonnes of scrap input. 