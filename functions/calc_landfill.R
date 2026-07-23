#' @title Landfill Plastic  
#' @param recyc_output Data frame output of the plastic collected based on recycling rate targets and adjusted for yield losses at 70%. 
#' @reference  CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25). 
#' @description
#' Calculates the plastic to landfill remaining after recycling output and incineration.  

calc_landfill <- function(wastegen, recyc_output, incineration)
{
  landfill <- wastegen |>
    filter(sector == 'all_sec') |>
    left_join(recyc_output, by = c("year")) |>
    left_join(incineration, by = "year") |>
    mutate(mt_plastic_landfill = mt_plastic_wastegen - mt_secondary_plastic_output - incin_mt) |>
    select(year, sector, mt_plastic_landfill)
  return(landfill)
}

