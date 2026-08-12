#' @title Forecast State Consumption 
#' @params State GDP SOURCE TBD 
#' @params State Population SOURCE TBD
# SAVE NEW DF with more accurate plastic intensities, slightly off due to rounding 

calc_forecast <- function(consum_2012_2020_total) {
  # read in North America Data Frame: replace in future ---------------------
  
  scaled_na_consumption <- read_csv(here::here("data", "raw", "scaled_na_consumption .csv")) 
  
  forecast_consum <- scaled_na_consumption |>
    mutate(year = as.character(year)) |>
    left_join(consum_2012_2020_total, by = "year") |> 
    mutate(total_consum_mt = total_consum_mt / 1000000) |> # convert metric tons to million metric tons
    mutate(total_consum_mt = case_when(
      as.numeric(year) > 2020 ~ (ca_gdp * plastic_intensity_kg_2017usd) / 1000000000, #kg & metric to million metric tons conversion 
      TRUE ~ total_consum_mt
    )) |>
   # select(year, total_consum_mt)
  
  return(forecast_consum)
}