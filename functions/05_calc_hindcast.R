# 05 Hindcast consumption  ------------------------------------------------
# calc_hindcast <- function (state_gdp, plastic_intensity)
# consumption_mt = state_gdp * plastic_intensity
#Forecast: only population & plastic intensity tons per dollar 
#Hindcast: US consumption data scaled between GDP and population & using 2012-2020 values to check IO estimate the fixed difference between the two 
#If only interested in packaging dont even need a hindcast because its used for WASTE GENERATION estimates today 
#Pottinger et al 2024 has the US hindcast complete consumption 
#io_frac_of_gdp = gdp_scaled - ca_io/ difference gdp_scaled & ca_io


calc_hindcast <- function(forecast_consum) {
  gdp_frac <- mean(forecast_consum$io_frac_of_gdp, na.rm = TRUE)
  
  consum_1950_2050 <- forecast_consum |>
    mutate(total_consum_mt = if_else(
      year < 2012,
      gpd_scaled - gdp_frac * diff_gdp_pop,
      total_consum_mt
    )) |> # typo in df gpd_scaled
    select(year, total_consum_mt)
  
  return(consum_1950_2050)
}
    

