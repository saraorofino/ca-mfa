

# add inputs for parameters based on scenario name ------------------------
###### add in inputs to policy options with _1, _2 etc 
scenario_param_inputs <- function(suffix, scenario_name) {
  base_inputs <- tagList(
    numericInput(paste0("policy_rate_", suffix), "Policy rate (%)",
                 value = 5, min = 0, max = 100, step = 0.5),
    numericInput(paste0("implement_year_", suffix), "Implementation year",
                 value = 2025, min = 2020, max = 2050, step = 1),
    numericInput(paste0("target_year_", suffix), "Target year",
                 value = 2035, min = 2020, max = 2050, step = 1)
  )
  
  if (scenario_name == "Source Reduction") {
    base_inputs <- tagList(
      base_inputs,
      numericInput(paste0("baseline_year_", suffix), "Baseline year",
                   value = 2023, min = 2010, max = 2050, step = 1)
    )
  }
  
  base_inputs
}


# Render UI ---------------------------------------------------------------

output$scenario_1_params_ui <- renderUI({
  req(input$scenario_name_1)
  if (input$scenario_name_1 == "BAU") return(NULL)
  scenario_param_inputs("1", input$scenario_name_1)
})