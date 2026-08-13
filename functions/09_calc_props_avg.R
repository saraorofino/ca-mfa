

calc_props_avg <- function(a_sector_consum) {
  

# create totals by sector for each year -----------------------------------
  sector_total <- a_sector_consum |>
    filter(!is.na(plastic_sector)) |>
    summarize(
      total_annual_sector = sum(tier_1_consum, tier_2_consum, tier_3_consum, tier_4_consum, oem_mt_consum, f_326_consum),
      .by = c(year, plastic_sector)
    )

# get totals across all sectors per year -----------------------------------

  year_totals <- sector_total |>
    distinct(year, plastic_sector, total_annual_sector) |>
    summarize(annual_total = sum(total_annual_sector), .by = year)
  
# find the proportion for each sector by year -----------------------------

  sector_props <- sector_total |>
    left_join(year_totals, by = "year", relationship = "many-to-many") |>
    mutate(props = total_annual_sector/ annual_total)

  check_props <- sector_props |>
    summarize(prop_check = sum(props), .by = year)
  
# find the average across all years for final proportions -----------------

  avg_props <- sector_props |>
    distinct(year, plastic_sector, props) |>
    summarize(avg_prop = mean(props), .by = plastic_sector)
  
  sum(avg_props$avg_prop) # check proportions 
  
  return(avg_props)
}