#' @title Build comparison tab lollipop plot
#' @param 
#' @description
#' Builds a lollipop chart comparing BAU, policy A, and policy B. Compares End of Life management (landfill, recycling), total production, and secondary plastics produced



build_comparison_lollipop <- function(eol_bau_data, eol_a_data, eol_b_data,
                                      total_consumption_bau, total_consumption_a, total_consumption_b,
                                      scenario_a_name, scenario_b_name,
                                      implement_year_a, implement_year_b,
                                      scenario_a_color, scenario_b_color){
  
  #internal function: summarize 3 dataframes
  
  summarize_comparison_data <- function(df, implement_year, total_consumption) { #nesting a function which will summarize each of the 3 dataframes 
    df |>
      filter(year >= implement_year) |> 
      summarise(
        landfill  = sum(mt_plastic_landfill, na.rm = TRUE),
        recycling = sum(mt_secondary_plastic_output, na.rm = TRUE),
        incineration = sum(mt_incin, na.rm = TRUE)
      ) |>
      pivot_longer(everything(), names_to = "category", values_to = "mt_plastic") # |> 
        
    #removing the consumption comparison from this plot, leaving comment incase it needs to be added back
      #bind_rows(
       # tibble(category = "consumption", mt_plastic = total_consumption)
      #)
  }
  

# building the combined dataframe for plotting  
  
  summarized_comparison_data <- bind_rows( # using summarize_comparison_data function on all 3 datasets and binding rows
    summarize_comparison_data(eol_bau_data, min(implement_year_a, implement_year_b), total_consumption_bau) |> mutate(scenario = "Business as Usual"), #using the minimum implementation year of 2 custom policies for BAU (for comparison)
    summarize_comparison_data(eol_a_data, implement_year_a, total_consumption_a) |> mutate(scenario = scenario_a_name),
    summarize_comparison_data(eol_b_data, implement_year_b, total_consumption_b) |> mutate(scenario = scenario_b_name)
  ) |> 
  mutate(
    category = factor(category, levels = c("incineration", "recycling", "landfill")),
    scenario = factor(scenario, levels = c("Business as Usual", scenario_a_name, scenario_b_name)),
    y_num = as.numeric(category) + case_when( #adding distance between lollipops
      scenario == "Business as Usual" ~ 0.2,
      scenario == scenario_a_name     ~ 0, 
      scenario == scenario_b_name     ~ -0.2
    )
  )

  #creating the plot 

  fill_values <- setNames(
    c("black", scenario_a_color, scenario_b_color),
    c("Business as Usual", scenario_a_name, scenario_b_name)
  )
  
  ggplot(summarized_comparison_data, aes(x = mt_plastic, y = y_num, color = scenario, fill = scenario)) +
    geom_segment(aes(x = 0, xend = mt_plastic, y = y_num, yend = y_num), linewidth = 1) +
    geom_point(shape = 21, size = 4, color = "black") +
    scale_fill_manual(values = fill_values) +
    scale_color_manual(values = fill_values) +
    scale_y_continuous(
      breaks = 1:nlevels(summarized_comparison_data$category),
      labels = c("Incineration", "Recycling", "Landfill")
    ) +
    labs(
      x = "Total Plastic (Mt)",
      y = NULL,
      fill = "Scenario"
    ) +
    theme_classic(base_family = "Times New Roman", base_size = 20) +
    theme(legend.position = "right") +
    guides(color = "none")
  
  
  
}
  
