#' @title Deflated Plastic Intensity Function v2 
#' @description These values match CAEEIO_326_output_2012_2020_v4

library(dplyr)

calc_deflated_plastic_int_v2<- function(rho_data) {
  m_2020 <- 1752
  
  # Helper to pull a single rho value for a given year
  get_rho <- function(yr) {
    val <- rho_data$rho[rho_data$year == yr & rho_data$bea_industry != "326/RoUS"]
    if (length(val) == 0) stop("No rho value found for year ", yr)
    return(val)
  }
  
  # Calculate m for every year 2012-2019 using m_2020 / rho_year
  other_years <- c(2012:2019)
  
  m_df <- tibble(year = other_years) |> 
    rowwise() |> 
    mutate(
      rho = get_rho(year),
      m   = m_2020 / rho
    ) |> 
    ungroup()
  
  # Step 3: Add back the known/anchor years for a complete 2012-2020 series
  m_df <- bind_rows(
    m_df,
    tibble(year = 2020, rho = get_rho(2020), m = m_2020),
  ) |> 
    arrange(year)
  
  return(m_df)
}