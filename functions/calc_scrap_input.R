#' @title Secondary Plastic Input for Recycled Content Targets
#' @param rc_perc_byo Data frame output of secondary plastic consumption from the recycled content policy.
#' @param recyc_yield Material loss rate from gathering recycled plastic to finished recycled content product.
#' @param ca_scrap_consump In-state percent of scrap consumption.
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors.
#' @details
#' Plastic Recycling Yield of 70% widely used assumption, see references below:
#' @references Geyer, R., Jambeck, J.R., Law, K.L., 2017. Production, use, and fate of all plastics ever made. Science Advances 3, e1700782. https://doi.org/10.1126/sciadv.1700782
#' @references Plastics Europe, 2024. The Circular Economy for Plastics—A European Analysis 2024. https://plasticseurope.org/knowledge-hub/the-circular-economy-for-plastics-a-european-analysis-2024/
#' @references  Pottinger, A.S., Geyer, R., Biyani, N., Martinez, C.C., Nathan, N., Morse, M.R., Liu, C., Hu, S., de Bruyn, M., Boettiger, C., Baker, E., McCauley, D.J., 2024. Pathways to reduce global plastic waste mismanagement and greenhouse gas emissions by 2050. Science 386, 1168–1173. https://doi.org/10.1126/science.adr3837
#' @description
#' This function calculates scrap input to the state using the post consumer recycled content production target consumption to work backwards with the recycling yield loss to find the total amount of recycled plastic input.
#' @return If summary 'FALSE' a data frame 'scrap_input" with columns for year, sector, and megatons of scrap input.
#' @return If summary 'TRUE', a data frame 'summarized' with 1950-2050 cumulative, in-state and out-of-state scrap input in megatons.


calc_scrap_input <- function(rc_perc, is_scrap_consump, summary = FALSE) {
  detailed <- rc_perc  |>
    mutate(scrap_input = mt_plastic_rc / 0.7) # recycling yield is 0.7
  
  if (!summary) {
    return(detailed)
  }
  summarized <- detailed |>
    filter(sector != "all_sec") |># removes all sector totals per year
    summarise(total_scrap = sum(scrap_input)) |>
    mutate(
      scrap_is = (total_scrap * is_scrap_consump),
      scrap_oos = total_scrap * (1 - is_scrap_consump)
    )
}


  
  
  
 