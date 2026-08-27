#' @title Build Consumption Line Chart
#' @param consum_bau dataframe output from calc_consum_bau
#' @param scenario_data dataframe output from chosen scenario, eg. sr, sb54, etc.
#' @param implement_year the implementation year of the chosen policy
#' @param scenario_label the label to show in the legend for the scenario line/ribbon, eg. "Source Reduction", "SB54"
#' @description a line chart which shows both business as usual consumption and consumption under the chosen scenario, with a ribbon to highlight the difference.
build_consum_line_chart <- function(consum_bau,
                                    scenario_data,
                                    implement_year,
                                    scenario_label) {
  
  plot_data <- consum_bau |>
    filter(sector == "all_sec") |>
    mutate(year = as.numeric(year)) |> 
    left_join(
      scenario_data |>
        filter(sector == "all_sec") |>
        mutate(year = as.numeric(year)) |> 
        select(year, mt_plastic_sr),
      by = "year"
    )
  
  color_values <- setNames(c("black", "#687E03"), c("Business as Usual", scenario_label))
  
  ggplot(plot_data, aes(x = year)) +
    geom_ribbon(
      aes(ymin = mt_plastic_sr, ymax = mt_plastic_bau),
      fill = "#687E03",
      alpha = 0.2
    ) +
    geom_line(aes(y = mt_plastic_bau, color = "Business as Usual"), linewidth = 0.8) +
    geom_line(aes(y = mt_plastic_sr, color = scenario_label), linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = implement_year, linetype = "dotted") +
    annotate(
      "text",
      x = implement_year,
      y = Inf,
      label = "Implementation Year",
      vjust = 1.6,
      hjust = 1.1,
      size = 5
    ) +
    scale_color_manual(values = color_values) +
    labs(x = "Year",
         y = "Annual Plastic Production (Mt)",
         color = "Scenario") +
    theme_classic(base_family = "Times New Roman", base_size = 16)
}