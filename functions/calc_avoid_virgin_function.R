
avoid_virgin_function <- function(consum_total_byo, rc_perc_byo) {
  consum_total_byo_54_clean %>%
    left_join(rc_perc_byo, by = c("year", "sector")) %>%
    mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
    select(year, sector, mt_plastic_virgin)
}