#' @title Consumption by Sector from Leontief Matrix 
#' # DO NOT USE L MATRIX FOR SECTOR STEP ONE 
#' 
calc_leontief_sector_consum <- function(consum_2012_2020, props) {
  plastic_consum <- consum_2012_2020 |>
    mutate(bea_sector_clean = str_remove(purchasing_industry, "/US-.*$"))
  
  props_clean <- props |>
    mutate(bea_sector_clean = str_remove(bea_sector, "\\.0$"))
  
  plastic_consum <- plastic_consum |>
    left_join(props_clean, by = "bea_sector_clean", relationship = "many-to-many") |>
    mutate(plastic_sector_consum = us_consumption * prop)
  
  consum_sector_2012_2020_Leontief <- plastic_consum |>
    group_by(plastic_sector, year) |>
    summarize(
      sector_consumption = sum(plastic_sector_consum, na.rm = TRUE),
      .groups = "drop"
    )
}