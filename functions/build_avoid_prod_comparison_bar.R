#' @title Build avoided production comparison bar.
#' @param avoid_prod_a the avoided production from policy a, pulled from get_avoid_prod function
#' @param avoid_prod_b the avoided production from policy b, pulled from get_avoid_prod function
#' @param scenario_a_name the name of scenario a, pulled from policy labels based on input
#' @param scenario_b_name the name of scenario b, pulled from policy labels based on input
#' @param scenario_color hex code for chosen scenario
#' @description: Builds a bar chart which compares the avoided virgin plastic production compared to business as usual (BAU) between policy A and Policy B, selected from inputs in the 'compare solution

build_avoid_prod_comparison_bar <- function(avoid_prod_a, avoid_prod_b,
                                            scenario_a_name, scenario_b_name,
                                            scenario_a_color, scenario_b_color) {
  
  avoid_prod_data <- tibble(
    Scenario = c(scenario_a_name, scenario_b_name),
    mt_plastic = c(avoid_prod_a, avoid_prod_b)
  ) |>
    mutate(
      Scenario = factor(Scenario, levels = c(scenario_a_name, scenario_b_name))
    )
  
  fill_values <- setNames(c(scenario_a_color, scenario_b_color), c(scenario_a_name, scenario_b_name))
  
  ggplot(avoid_prod_data, aes(x = Scenario, y = mt_plastic, fill = Scenario)) +
    geom_col(width = 0.5) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = NULL,
      y = "Avoided Virgin Plastic Production (Mt)"
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 20) +
    theme(legend.position = "none")
}