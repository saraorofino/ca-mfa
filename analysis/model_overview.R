# Rough draft for Server.R 


# State Selection for BAU EEIO consumption model --------------------------------

state_choices <- c("AK","AL","AR","AS","AZ","CA","CO","CT","DC","DE","FL","GA","GU","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MP","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UM","UT","VA","VI","VT","WA","WI","WV","WY")

consum_bau <- reactive({
  req(input$state)
  calc_consum_bau(input$state) # pulls in pre-processing, eeio model 
})

# should this output a df saved to global environment???? 


# Map Policy Scenarios to Scripts ------------------------------------------------

policy_registry <- list(
  "Source Reduction" = "R/run_policy_sr.R",
  "Recycling Rate" = "R/run_policy_rr.R",
  "Recycled Content" = "R/run_policy_rc.R",
  "SB54" =  "R/run_policy_sb54.R",
  "Comprehensive Policy" = "R/run_policy_comp.R",
  # comprehensive
  "BAU"      = "R/run_bau.R" # output data frame of policy scenarios sourcing consum_bau
)


# Run Scenario Function ------------------------------------------------------------
# Each script defines run_policy(params). params is a list of
# policy_rate / implement_year / target_year or NUll for bau )
# State BAU data will already be calculated in preprocessing from the state drop down option

run_scenario <- function(scenario_name, params = NULL, consum_bau_df) { # pass consum bau output here to pull into the new environment, resets if changed in user inputs
  script_path <- scenario_registry[[scenario_name]] #pulls correct analysis script 
  env <- new.env() # Source into new environment to avoid issues 
  source(script_path, local = env) # pull the policy_analysis script 
  result <- env$run_policy(params)
  result$scenario <- scenario_name
  result
}


# Run Scenario 1 & 2 REACTIVE ---------------------------------------------

scenario_1 <- reactive({run_scenario(input$scenario_name_1, input$params_1)})

scenario_2 <- reactive({run_scenario(input$scenario_name_2, input$params_2)})

# Comparison & Outputs Function  -------------------------------------------------------
# takes the raw inputs (both scenarios, both scenarios' param lists) and produces the analysis.
# Stays a plain function so it can be called/tested outside of reactivity.

run_comparison <- function(scenario_1, scenario_2) {
  validate(need(scenario_1 != scenario_2,
                "Scenario 1 and Scenario 2 must be different."))
  
# Table outputs & graph placeholder 

# Virgin Plastic Production Difference ------------------------------------


# Plastic Waste Disposal Difference--------------------------------------------------


# GHG Difference ----------------------------------------------------------

 
}




