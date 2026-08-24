#' @title Build Recycled Content Consumption Comparison Lollipop Plot
#' @param consum_bau Dataframe output from consum_bau function
#' @param avoid_prod_rc Dataframe output from run_policy_rc
#' @param scenario_color The hex code color for the recycled content scenario
#' @description Builds a lollipop chart which compares virgin production in BAU and RC scenarios

build_rc_comparison_plot <- function(consum_bau, avoid_prod_rc, scenario_color) {
  
  bau_total <- consum_bau |> 
    filter(sector == "all_sec") |> 
    summarise(total = sum(mt_plastic_bau, na.rm = TRUE)) |> 
    pull(total)
  
  rc_data <- tibble(
    Scenario = c("Business As Usual", "Recycled Content Intervention"),
    mt_plastic = c(bau_total, bau_total - avoid_prod_rc)
  ) |>
    mutate(
      Scenario = factor(Scenario, levels = c("Recycled Content Intervention", "Business As Usual")),
      y_num = as.numeric(Scenario) * 0.3 #adding a line spacing for visual
    )
  
  fill_values <- setNames(c(scenario_color, "black"), c("Recycled Content Intervention", "Business As Usual")) #bau remains same across charts, change here if needed
  
  ggplot(rc_data, aes(x = mt_plastic, y = y_num, color = Scenario, fill = Scenario)) +
    geom_segment(aes(x = 0, xend = mt_plastic, y = y_num, yend = y_num), linewidth = 1) +
    geom_point(shape = 21, size = 4, color = "black") +
    scale_fill_manual(values = fill_values) +
    scale_color_manual(values = fill_values) +
    scale_y_continuous(
      breaks = rc_data$y_num,
      labels = rc_data$Scenario,
      limits = c(0, max(rc_data$y_num) + 0.3) #adjusting the line spacing
    ) +
    labs(
      x = "Total Virgin Plastic Production (Mt)",
      y = NULL
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 16) +
    theme(legend.position = "none") +
    guides(color = "none")
}

