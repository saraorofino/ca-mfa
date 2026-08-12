# leontief * f / deflated plastic


calc_state_consum <- function(state_abbr, deflated_plastic_intensity,
                              consumption_element) {
  
  df_name <- paste0(state_abbr, "_long")
  if (!exists(df_name)) {
    stop("Object '", df_name, "' not found. Run download_rds_state_model('",
         state_abbr, "') first.")
  }
  state_long <- get(df_name)
  
  # ---- L values for row 326, all purchasing industries, all years ----------
  leontif_326 <- state_long |>
    filter(element == "L", row == paste0("326/US-", state_abbr)) |>
    select(purchasing_industry = col, year, leontief = value)
  
  # ---- consumption values by industry and year -------------------------------
  us_consum <- state_long |>
    filter(element == consumption_element) |>
    select(purchasing_industry = row, year, us_consumption = value)
  
  # ---- ensure consistent year type before joining ----------------------------
  deflated_plastic_intensity <- deflated_plastic_intensity |>
    mutate(year = as.character(year))
  
  # ---- join L, consumption, and m (deflated plastic intensity) --------------
  state_consum <- left_join(leontif_326, us_consum,
                            by = c("purchasing_industry", "year"))
  
  state_consum <- left_join(state_consum, deflated_plastic_intensity, by = "year") |>
    mutate(state_consum_mt = (leontief * us_consumption) / m)
  
  return(state_consum)
}

