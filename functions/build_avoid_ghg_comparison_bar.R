#' @title Build avoided ghg comparison bar.
#' @param avoid_ghg_a the avoided production from policy a, pulled from get_avoid_prod function
#' @param avoid_ghg_b the avoided production from policy b, pulled from get_avoid_prod function
#' @param scenario_a_name the name of scenario a, pulled from policy labels based on input
#' @param scenario_b_name the name of scenario b, pulled from policy labels based on input
#' @param scenario_color hex code for chosen scenario
#' @description: Builds a bar chart which compares the avoided virgin plastic production compared to business as usual (BAU) between policy A and Policy B, selected from inputs in the 'compare solution

build_avoid_ghg_comparison_bar <- function(avoid_ghg_a, avoid_ghg_b,
                                            scenario_a_name, scenario_b_name,
                                            scenario_a_color, scenario_b_color) {
  
  avoid_ghg_data <- tibble(
    Scenario = c(scenario_a_name, scenario_b_name),
    mt_co2e = c(avoid_ghg_a, avoid_ghg_b)
  ) |>
    mutate(
      Scenario = factor(Scenario, levels = c(scenario_a_name, scenario_b_name))
    )
  
  fill_values <- setNames(c(scenario_a_color, scenario_b_color), c(scenario_a_name, scenario_b_name))
  
  ggplot(avoid_ghg_data, aes(x = Scenario, y = mt_co2e, fill = Scenario)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = NULL,
      y = "Avoided GHG Emissions (Mt CO2e)"
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 20) +
    theme(legend.position = "none")
}