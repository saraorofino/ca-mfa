#' @title Collected Recycling
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @reference CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25).
#' @description
#' Calculates the weight of recycling collected in each sector, currently only packaging recycled, per year. Uses the waste generation per year dataframe (wastegen) and the static recycling collection rate under business as usual dataframe. (bau_rr). For business-as-usual pre-preprocessing leave out implement_year_rr, targer_rr and target_year_rr and ensure wastegen fed into the function is for BAU waste generation.



calc_collect_recyc <- function(wastegen,
                               bau_rr_sect,#->ca_rr_pack (make sure to update code)
                               implement_year_rr,
                               target_rr,
                               target_sector_rr,
                               target_year_rr)
{
  # Rows that are NOT the target sector — pass through unchanged in both branches
  other_sectors <- wastegen |>
    filter(sector != target_sector_rr, sector != "all_sec") |>
    mutate(mt_plastic_collect = 0)
  
  # function to rebuild all_sec rows
  build_all_sec <- function(target_df, other_df) {
    totals <- bind_rows(target_df, other_df) |>
      group_by(year) |>
      summarise(
        mt_plastic_collect = sum(mt_plastic_collect, na.rm = TRUE),
        .groups = "drop"
      )
    
    wastegen |>
      filter(sector == "all_sec") |>
      select(-any_of("mt_plastic_collect")) |>
      left_join(totals, by = "year")
  }
  
  if (missing(implement_year_rr) &
      missing(target_rr) & missing(target_year_rr)) {
    collect_recyc_bau <- wastegen |>
      filter(sector == target_sector_rr) |>
      left_join(bau_rr, by = "year") |>
      mutate(mt_plastic_collect = mt_plastic_wastegen * bau_rr)
    
    all_sec_row <- build_all_sec(collect_recyc_bau, other_sectors)
    
    collect_recyc_bau <- bind_rows(collect_recyc_bau, other_sectors, all_sec_row)
    
    return(collect_recyc_bau)
  }
  
  
  baseline_year_rr <- implement_year_rr   # baseline year = implementation year itself
  
  baseline_rate <- bau_rr |>
    filter(year == baseline_year_rr) |>
    pull(bau_rr)
  
  wastegen_target <- wastegen |>
    filter(sector == target_sector_rr) |> #change to all_sec
    left_join(bau_rr, by = "year") #bau_rr column u not W (8.6 not 19)
  
  collect_recyc <- wastegen_target |>
    mutate(
      rr_multiplier = case_when(
        target_rr <= baseline_rate ~ bau_rr,
        year <= baseline_year_rr ~ bau_rr,
        year > baseline_year_rr & year <= target_year_rr ~
          baseline_rate + (target_rr - baseline_rate) * (year - baseline_year_rr) / (target_year_rr - baseline_year_rr),
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
  
  
  all_sec_row <- build_all_sec(collect_recyc, other_sectors)
  
  collect_recyc <- bind_rows(collect_recyc, other_sectors, all_sec_row)
  
  return(collect_recyc)
}
