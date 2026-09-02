

build_avoid_ghg_comparison_bar <- function(avoid_ghg_a, avoid_ghg_b,
                                            scenario_a_name, scenario_b_name,
                                            scenario_a_color, scenario_b_color) {
  
  avoid_prod_data <- tibble(
    Scenario = c(scenario_a_name, scenario_b_name),
    mt_co2e = c(avoid_prod_a, avoid_prod_b)
  ) |>
    mutate(
      Scenario = factor(Scenario, levels = c(scenario_a_name, scenario_b_name))
    )
  
  fill_values <- setNames(c(scenario_a_color, scenario_b_color), c(scenario_a_name, scenario_b_name))
  
  ggplot(avoid_prod_data, aes(x = Scenario, y = mt_co2e, fill = Scenario)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = NULL,
      y = "Avoided GHG Emissions (Mt CO2e)"
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 20) +
    theme(legend.position = "none")
}