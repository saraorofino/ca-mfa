# leontief * f / deflated plastic




calc_state_consum <- function(leontif_326,
                              us_consum_2012_2020,
                              deflated_plastic_intensity) {
  state_consum <- left_join(leontif_326,
                            us_consum_2012_2020,
                            by = c("purchasing_industry", "year"))
  
  state_consum <- left_join(state_consum, deflated_plastic_intensity, by = "year") |>
    mutate(state_consum_mt = (leontif * us_consumption_complete) / m)
}
