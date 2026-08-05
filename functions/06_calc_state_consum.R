# leonif * f / deflated plastic

calc_state_consum <- function(leontif_326, complete_consumption, deflated_plastic_intensity) {
  state_consum <- left_join(leontif_326, complete_consumption, by = "purchasing_industry") |>
    mutate(state_consum = (leontief*complete_consumption)/ deflated_plastic_intensity)
}