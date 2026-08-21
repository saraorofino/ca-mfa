#' @title Build EOL Comparison Lollipop Plot
#' @param eol_data Datafram output from build_eol_comparison_data function
#' @param scenario_name The name of the scenario for the label
#' @param scenario_color The hex code color for the chosen scenario
#' @description Takes the output of build_eol_comparison_data() and renders the BAU-vs-scenario lollipop chart.
  
build_eol_comparison_plot <- function(eol_data, scenario_name, scenario_color, plot_title) {
  
  fill_values <- setNames(c("#A8A8A8", scenario_color), c("BAU", scenario_name)) #bau remains same across charts, change here if needed
  
  ggplot(eol_data, aes(x = mt_plastic, y = y_num, color = scenario, fill = scenario)) +
    geom_segment(aes(x = 0, xend = mt_plastic, y = y_num, yend = y_num), linewidth = 0.8) +
    geom_point(shape = 21, size = 4, color = "black") +
    scale_fill_manual(values = fill_values) +
    scale_color_manual(values = fill_values) +
    scale_y_continuous(
      breaks = 1:nlevels(eol_data$eol_type),
      labels = c("Recycling", "Landfill")
    ) +
    labs(
      title = plot_title,
      x = "Total Plastic Waste (Million Metric Tons)",
      y = NULL,
      fill = "Scenario"
    ) +
    theme_classic(base_family = "Times New Roman") +
    theme(legend.position = "right") +
    guides(color = "none")
}