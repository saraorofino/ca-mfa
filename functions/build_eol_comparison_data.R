#' @title: Building the end of life comparison data
#' @param eol_bau_data: End of Life data output from run_bau
#' @param eol_scenario_data: End of life data output from rol_policy_(scenario).
#' @param scenario_name: The scenario name, used to label the scenario
#' @param implement_year: The implement year, to only show data from implement year - 2050
#' @description: Builds a comparison data frame with the end of life measures (landfill, recycling, incineration) for both business as usual and a the changeable input scenario. Used for creating the comparison lollipop chart.




build_eol_comparison_data <- function(eol_bau_data, eol_scenario_data, scenario_name, implement_year) {
  
  summarize_eol <- function(df) {
    df |>
      filter(year >= implement_year) |> 
      summarise(
        landfill  = sum(mt_plastic_landfill, na.rm = TRUE),
        recycling = sum(mt_secondary_plastic_output, na.rm = TRUE)
      ) |>
      pivot_longer(everything(), names_to = "eol_type", values_to = "mt_plastic") |>
      mutate(proportion = mt_plastic / sum(mt_plastic))
  }
  
  bind_rows(
    summarize_eol(eol_bau_data) |> mutate(scenario = "BAU"),
    summarize_eol(eol_scenario_data) |> mutate(scenario = scenario_name)
  ) |>
    mutate(
      eol_type = factor(eol_type, levels = c("recycling", "landfill")),
      y_num = as.numeric(eol_type) + ifelse(scenario == "BAU", 0.15, -0.15)
    )
}