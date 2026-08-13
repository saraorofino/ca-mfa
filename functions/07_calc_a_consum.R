#' @title Calculate A matrix consumption from USD to mt plastic
#' @description Plastic consumption conversion by purchasing tier from USD to million metric tons. 
#' @param m, Plastic intensity to inflate, deflate values to 2020. 
#' @param power_series Power series calculating consumption from final demand, f, from EPA Complete Consumption for BEA sector 326 and A matrix values
#' @param state_model EPA environmentally extended input-output model 


calc_a_consum <- function(state_model, power_series, m) {
  # Pull 2020 state rho values for all years in model -----------------------
  rho_326 <- state_model |>
    filter(element == "rho", startsWith(row, "326/US"), year == "2020") |>
    select(col, rho_326 = value)
  
  m <- m |>
    select(year, m) |>
    mutate(year = as.character(year))
  
  a_consum <- power_series |>
    left_join(rho_326, by = c("year" = "col")) |>
    left_join(m, by = "year") |>
    mutate(
      total_mt  = leontief_326 * final_demand / m,
      f_326 = demand_326 / m,
      oem_mt    = a_326 * final_demand / m,
      tier_1_mt = a_326 * a1f / m,
      tier_2_mt = a_326 * a2f / m,
      tier_3_mt = a_326 * a3f / m,
      tier_4_mt = a_326 * a4f / m
    ) |>
    select(year, row, total_mt, oem_mt, f_326, tier_1_mt, tier_2_mt, tier_3_mt, tier_4_mt)
  
  return(a_consum)
}

