# leontief * f / deflated plastic

#For these I would use the file as an input so you can apply the function to every file in a folder and pull the same information and #then combine it into one long dataframe with year, l_326, final_demand (or something like that).


calc_state_consum <- function(leontif_326,
                              us_consum_2012_2020,
                              deflated_plastic_intensity) {
  state_consum <- left_join(leontif_326,
                            us_consum_2012_2020,
                            by = c("purchasing_industry", "year"))
  
  state_consum <- left_join(state_consum, deflated_plastic_intensity, by = "year") |>
    mutate(state_consum_mt = (leontif * us_consumption_complete) / m)
}
