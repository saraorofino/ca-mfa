#' @title Build Recycled Content Consumption Comparison Bar Chart
#' @param consum_bau Dataframe output from consum_bau function
#' @param avoid_prod_rc Dataframe output from run_policy_rc
#' @param scenario_color The hex code color for the recycled content scenario
#' @description Builds a horizontal bar chart which compares virgin production in BAU and RC scenarios



build_rc_bar_plot <- function(consum_bau, avoid_prod_rc, scenario_color) {
  
  bau_total <- consum_bau |> 
    filter(sector == "all_sec") |> 
    summarise(total = sum(mt_plastic_bau, na.rm = TRUE)) |> 
    pull(total)
  
  rc_data <- tibble(
    Scenario = c("Business As Usual", "Recycled Content Intervention"),
    mt_plastic = c(bau_total, bau_total - avoid_prod_rc)
  ) |>
    mutate(
      Scenario = factor(Scenario, levels = c("Recycled Content Intervention", "Business As Usual"))
    )
  
  fill_values <- setNames(c(scenario_color, "black"), c("Recycled Content Intervention", "Business As Usual")) #bau remains same across charts, change here if needed
  
  ggplot(rc_data, aes(x = mt_plastic, y = Scenario, fill = Scenario)) +
    geom_col(width = 0.3) +
    scale_fill_manual(values = fill_values) +
    labs(
      x = "Total Virgin Plastic Production (Mt)",
      y = NULL
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 16) +
    theme(legend.position = "none") +
    guides(color = "none")
}