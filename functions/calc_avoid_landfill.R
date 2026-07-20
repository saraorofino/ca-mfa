#' @title Avoided Landfill Plastic 
#' @param recyc_output Data frame output of the plastic collected based on recycling rate targets and adjusted for yield losses at 70%. 
#' @reference  CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25). 
#' @description
#' Calculates the plastic to landfill remaining after recycling output and incineration.  

calc_avoid_landfill <- function(landfill, landfill_bau) #landfill and #landfill_bau 
{
  avoid_landfill <- landfill |>
    filter(sector == 'pack') |>
    left_join(landfill_bau, by = c("year", "sector")) |>
    mutate(mt_avoid_landfill = landfill_bau$mt_plastic_landfill - landfill$mt_plastic_landfill) |>
    select(year, sector, mt_avoid_landfill)
  return(avoid_landfill)
}


