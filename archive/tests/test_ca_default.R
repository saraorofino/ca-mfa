ca_consum_bau_default <- calc_consum_bau(
  bea_to_plastic = bea_to_plastic,
  state_abbr = "CA",
  consumption_element = "Consumption_Complete",
  scaled_na_consumption = scaled_na_consumption,
  n_iterations = 4
)

saveRDS(ca_consum_bau_default, here::here("data", "static", "ca_consum_bau_default.rds"))