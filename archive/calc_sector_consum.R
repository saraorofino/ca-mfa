## by A matrix consumption

calc_sector_consum <- 
power_series_clean <- power_series |>
  mutate(bea_sector_clean = str_remove(row, "/US-.*$")) |>
  left_join(props_clean, by = "bea_sector_clean")

tier_1 <- power_series_clean |>
  mutate(tier_1_consum = af1 * prop) |>
  group_by(plastic_sector, year) |>
  summarize(tier_1_consum = sum(tier_1_consum, na.rm = TRUE), .groups = "drop")

tier_2 <- power_series_clean |>
  mutate(tier_2_consum = af2 * prop) |>
  group_by(plastic_sector, year) |>
  summarize(tier_2_consum = sum(tier_2_consum, na.rm = TRUE), .groups = "drop")

tier_3 <- power_series_clean |>
  mutate(tier_3_consum = af3 * prop) |>
  group_by(plastic_sector, year) |>
  summarize(tier_3_consum = sum(tier_3_consum, na.rm = TRUE), .groups = "drop")

tier_4 <- power_series_clean |>
  mutate(tier_4_consum = af4 * prop) |>
  group_by(plastic_sector, year) |>
  summarize(tier_4_consum = sum(tier_4_consum, na.rm = TRUE), .groups = "drop")

consum_sector_2012_2020_A_Matrix <- tier_1 |>
  left_join(tier_2, by = c("plastic_sector", "year")) |>
  left_join(tier_3, by = c("plastic_sector", "year")) |>
  left_join(tier_4, by = c("plastic_sector", "year"))