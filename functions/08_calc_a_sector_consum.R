### Add in consumptin for 326 x 326 to calc final demand (f) % m to get final consumption 326
### all other sectors get 0 in 326_final

calc_a_sector_consum <- function(a_consum, props) {
  props_clean <- props |>
    mutate(bea_sector_clean = str_remove(bea_sector, "\\.0$"))
  
  a_consum_clean_mt <- a_consum |>
    mutate(bea_sector_clean = str_remove(row, "/US-.*$")) |>
    left_join(props_clean, by = "bea_sector_clean", relationship = "many-to-many")
  
  props_clean <- props |>
    mutate(bea_sector_clean = str_remove(bea_sector, "\\.0$"))
  
  
  f_326 <- a_consum_clean_mt |>
    mutate(f_326 = f_326 * prop) |>
    group_by(plastic_sector, year) |>
    summarize(f_326_consum = sum(f_326, na.rm = TRUE),
              .groups = "drop")
  
  oem_mt <-  a_consum_clean_mt |>
    mutate(oem_mt = oem_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(oem_mt_consum = sum(oem_mt, na.rm = TRUE),
              .groups = "drop")
  
  tier_1 <- a_consum_clean_mt |>
    mutate(tier_1_consum = tier_1_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(tier_1_consum = sum(tier_1_consum, na.rm = TRUE),
              .groups = "drop")
  
  tier_2 <- a_consum_clean_mt |>
    mutate(tier_2_consum = tier_2_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(tier_2_consum = sum(tier_2_consum, na.rm = TRUE),
              .groups = "drop")
  
  tier_3 <- a_consum_clean_mt |>
    mutate(tier_3_consum = tier_3_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(tier_3_consum = sum(tier_3_consum, na.rm = TRUE),
              .groups = "drop")
  
  tier_4 <- a_consum_clean_mt |>
    mutate(tier_4_consum = tier_4_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(tier_4_consum = sum(tier_4_consum, na.rm = TRUE),
              .groups = "drop")
  
  total <- a_consum_clean_mt |>
    mutate(total_consum = total_mt * prop) |>
    group_by(plastic_sector, year) |>
    summarize(total_consum = sum(total_consum, na.rm = TRUE),
              .groups = "drop")
  
  consum_sector_2012_2020_A_Matrix <- tier_1 |>
    left_join(tier_2, by = c("plastic_sector", "year")) |>
    left_join(tier_3, by = c("plastic_sector", "year")) |>
    left_join(tier_4, by = c("plastic_sector", "year")) |>
    left_join(total, by = c("plastic_sector", "year")) |>
    left_join(oem_mt, by = (c("plastic_sector", "year"))) |>
    left_join(f_326, by = (c("plastic_sector", "year")))
}
