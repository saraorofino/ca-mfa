#' @title Business-As_Usual Model Projection
#' @description Creates a data frame for projected state plastic consumption from 1950 - 2050. For detailed methodology refer to the report. 
#' @source Roland Geyer, Sara Orofino, Eleanor Thomas, and Darcy Bradley (2025) Policy is Essential to Curb Plastic Pollution: The example of California’s Senate Bill 54. The Nature Conservancy, San Francisco, California, USA.



calc_consum_bau <- function(bea_to_plastic, state_abbr, consumption_element, scaled_na_consumption, n_iterations) {

  # 00 Sector percentages pre-processing ---------------------------------------
  props <- preprocess_sectors(bea_to_plastic=bea_to_plastic)
  
  # 01 Create State Long Data ---------------------------------------------
  download_rds_state_model(state_abbr = state_abbr) # Reactive input in shiny 

  # 02 Calculate deflated/inflated plastic intensity (m) ------------------------

  m <- calc_deflated_plastic_int(state_abbr = state_abbr) 


  # 03 Calculate State Consumption from Leontief Matrix, final demand and plastic intensity ( f * L / m) in metric tons -----------------------------

  consum_2012_2020_total <- calc_state_consum(state_abbr = state_abbr, deflated_plastic_intensity = m,
                                              consumption_element = consumption_element) 

  # 04 Forecast (do first) -------------------------------------------------------------
  # load in data; slope of change in plastic intensity from model data 2012-2020 to get change in plastic intensity 

  forecast_consum <- calc_forecast(consum_2012_2020_total, scaled_na_consumption=scaled_na_consumption)

  # 05 Hindcast consumption  ------------------------------------------------

  consum_1950_2050 <- calc_hindcast(forecast_consum)

  # 06 Calculate A matrix power series   -------------------------------------------------------

  power_series <- calc_power_series(state_abbr=state_abbr, n_iterations = n_iterations) 

  # 07 Calculate A consumption in million metric tons of plastic -------------------------------------------------------

  a_consum <-calc_a_consum(state_abbr=state_abbr, power_series, m) 


  # 08 average proportion of consumption by plastic sector ---------------------

  avg_props <- calc_plastic_sector_props(a_consum, props) 


  # 09 create consum_bau final data frame by sector --------------------------------

  consum_bau_wide <- calc_final_bau_consum(avg_props, consum_1950_2050)
  
  #10 pivot longer for functions
  consum_bau <- consum_bau_wide |>
    select(-total_consum_mt) |>            
    tidyr::pivot_longer(
      cols = -year,
      names_to = "sector",
      values_to = "mt_plastic_bau"
    )

  consum_bau_summary <- consum_bau |>  # add all_sec to data frame
    group_by(year) |>
    summarize(mt_plastic_bau = sum(mt_plastic_bau),
              .groups = "drop") |>
    mutate(sector = "all_sec")
  
  consum_bau <- bind_rows(consum_bau, consum_bau_summary)
  
  
return(consum_bau)

}





