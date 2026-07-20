#' @title Collected Recycling
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @reference CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25).
#' @description
#' Calculates the weight of recycling collected in each sector, currently only packaging recycled, per year. Uses the waste generation per year dataframe (wastegen) and the static recycling collection rate under business as usual dataframe. (bau_rr). For business-as-usual pre-preprocessing leave out implement_year_rr, targer_rr and target_year_rr and ensure wastegen fed into the function is for BAU waste generation. 


calc_collect_recyc <- function(wastegen,
                               bau_rr,
                               implement_year_rr,
                               target_rr,
                               target_sector_rr,
                               target_year_rr)
  
  
  
{
  if (missing(implement_year_rr) &
      missing(target_rr) & missing(target_year_rr)) {
    collect_recyc_bau <- wastegen |>
      filter(sector == target_sector_rr) |>
      left_join(bau_rr, by = "year") |>
      mutate(mt_plastic_collect = mt_plastic_wastegen * bau_rr)
    return(collect_recyc_bau)
  }
  
  if (target_year_rr <= implement_year_rr) {
    stop("target_year must be after implement_year")
  }
  baseline_rate <- bau_rr |>
    filter(year == (implement_year_rr - 1)) |>
    pull(bau_rr)
  
  wastegen <- wastegen |>
    filter(sector == target_sector_rr) |>
    left_join(bau_rr, by = "year")
  
  collect_recyc <- wastegen |>
    mutate(
      baseline_year = implement_year_rr - 1,
      rr_multiplier = case_when(
        target_rr <= baseline_rate ~ bau_rr,
        year <= baseline_year ~ bau_rr,
        year > baseline_year & year <= target_year_rr ~
          baseline_rate + (target_rr - baseline_rate) * (year - baseline_year) / (target_year_rr - baseline_year),
        year > target_year_rr ~ target_rr,
        TRUE ~ bau_rr
      )
    )  |>
    
    mutate(
      mt_plastic_collect = case_when(
        target_sector_rr == 'pack' &
          year > implement_year_rr ~ mt_plastic_wastegen * rr_multiplier,
        TRUE ~ mt_plastic_wastegen * bau_rr
      )
    )
  
  return(collect_recyc)
}
