#' @title Avoided Landfill Plastic 
#' @param eol Data frame output of mega metric tons of plastic by end of life including incineration, recycling and landfill fates. 
#' @reference  CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25). 
#' @description
#' Calculates the plastic to landfill remaining after recycling output and incineration.  

calc_avoid_landfill <- function(landfill, landfill_bau) #landfill and #landfill_bau 
{
  avoid_landfill <- eol |>
    filter(sector == 'pack') |>
    left_join(landfill_bau, by = c("year", "sector")) |>
    mutate(mt_avoid_landfill = eol_bau$mt_plastic_landfill - eol$mt_plastic_landfill) |>
    select(year, sector, mt_avoid_landfill)
  return(avoid_landfill)
}


