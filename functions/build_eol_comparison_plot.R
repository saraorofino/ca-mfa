#' @title Build EOL Comparison Lollipop Plot
#' @param eol_data Datafram output from build_eol_comparison_data function
#' @param scenario_name The name of the scenario for the label
#' @param scenario_color The hex code color for the chosen scenario
#' @description Takes the output of build_eol_comparison_data() and renders the BAU-vs-scenario lollipop chart.

build_eol_comparison_plot <- function(eol_data, scenario_name, scenario_color) {
  
  fill_values <- setNames(c("black", scenario_color), c("Business as Usual", scenario_name)) #bau remains same across charts, change here if needed
  
  ggplot(eol_data, aes(x = mt_plastic, y = y_num, color = scenario, fill = scenario)) +
    geom_segment(aes(x = 0, xend = mt_plastic, y = y_num, yend = y_num), linewidth = 1) +
    geom_point(shape = 21, size = 4, color = "black") +
    scale_fill_manual(name = "Scenario",
                      values = fill_values) +
    scale_color_manual(values = fill_values) +
    scale_y_continuous(
      breaks = 1:nlevels(eol_data$eol_type),
      labels = c("Incineration", "Recycling", "Landfill")
    ) +
    labs(
      x = "Total Plastic Waste (Mt)",
      y = NULL,
      fill = "Scenario"
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 20) +
    theme(legend.position = "right") +
    guides(color = "none")
}

