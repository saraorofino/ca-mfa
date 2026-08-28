#' @title Build Comparison GHG line chart
#' @description builds a line chart which shows a time series line chart of annual greenhouse gas emmisions, with 3 lines. one for business as usual, one for scenario A, one for scenario B


build_ghg_comparison_line_chart <- function(ghg_bau_data, ghg_a_data, ghg_b_data,
                                 scenario_a_name, scenario_b_name,
                                 implement_year_a, implement_year_b,
                                 scenario_a_color, scenario_b_color){
  
  #function to summarize annual GHG in each scenario:
  
  calc_ghg_total <- function(ghg_data){
    ghg_data$ghg_prod |>
      filter(sector == "all_sec") |> 
      select(year, mt_co2e_prod) |> 
      left_join(
        ghg_data$ghg_eol |> filter(sector == "all_sec") |> select(year, mt_co2e_eol),
        by = "year"
      ) |>
      left_join(
        ghg_data$ghg_avoid_prim_prod |> filter(sector == "all_sec") |> select(year, mt_co2e_avoidprod),
        by = "year"
      ) |>
      mutate(mt_co2e_total = mt_co2e_prod + mt_co2e_eol + mt_co2e_avoidprod) |>
      select(year, mt_co2e_total)
  }
  
  #using the above function on all 3 scenarios:
  
  bau_total <- calc_ghg_total(ghg_bau_data) |> rename(mt_co2e_bau = mt_co2e_total) #renaming columns to be standard for plot
  a_total   <- calc_ghg_total(ghg_a_data)   |> rename(mt_co2e_a = mt_co2e_total)
  b_total   <- calc_ghg_total(ghg_b_data)   |> rename(mt_co2e_b = mt_co2e_total)
  
  # joining all 3 together for a plot_data dataframe
  
  plot_data <- bau_total |> 
    left_join(a_total, by = "year") |> 
    left_join(b_total, by = "year") |> 
    mutate(year = as.numeric(year))
  
  color_values <- setNames(
    c("black", scenario_a_color, scenario_b_color),
    c("Business as Usual", scenario_a_name, scenario_b_name)
  )
  
  ggplot(plot_data, aes(x = year)) +
    geom_line(aes(y = mt_co2e_bau, color = "Business as Usual"), linewidth = 0.8) +
    geom_line(aes(y = mt_co2e_a, color = scenario_a_name), linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = mt_co2e_b, color = scenario_b_name), linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = implement_year_a, linetype = "dotted", color = scenario_a_color) +
    geom_vline(xintercept = implement_year_b, linetype = "dotted", color = scenario_b_color) +
    annotate(
      "text",
      x = 2024, #using 2024 in order for text to center on chart properly
      y = Inf,
      label = "Implementation Year",
      vjust = 1.6,
      hjust = 1.1,
      size = 5
    ) +
    scale_color_manual(values = color_values) +
    labs(x = "Year", y = "Annual GHG Emissions (Mt CO2e)", color = "Scenario") +
    theme_classic(base_family = "Times New Roman", base_size = 20)
  
  
}