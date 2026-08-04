
library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Plastic Policy Impact Model",
  id = "main_tabs",
  theme = bs_theme(version = 5),

# Side Panel: State Inputs ------------------------------------------------

  sidebar = sidebar(
    width = 200,
    
    selectInput(
      inputId = "state",
      label = "State:",
      choices = c(
        "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
        "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
        "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
        "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
        "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
        "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
        "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
        "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
        "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
        "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
        "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
        "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
        "Wisconsin" = "WI", "Wyoming" = "WY"
      ),
      selected = "CA"
    )
    
    # Additional shared inputs placeholder (e.g. choose recycling rate CA or National Average, incineration)
    # They will also persist across tabs.
  ),
  

# Overview ----------------------------------------------------------------

  nav_panel(
    "Overview",
    br(),
    h4("Policy Options"),
    br(),
    h6(
      "Insert summary about source reduction, recycled content, recycling rate, combination and SB54"
    ),
    br(),
    h4("Business as Usual Production 1950-2050"),
    tableOutput("overview_summary_table"),
    br(),
    h4("Business as Usual by Sector"),
    plotOutput("bau_overview_plot")
  ),
  
  # ---------------- Individual Policy (with sub-tabs) ----------------
  nav_panel(
    "Individual Policy",
    br(),
    tabsetPanel(
      id = "individual_policy_tabs",
      tabPanel(
        "Source Reduction",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_sr", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("baseline_year_sr", "Baseline Year:", choices = 1950:2025, selected = 2023),
            selectInput("target_year_sr", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_sr", "Implement Year:", choices = 2026:2050, selected = 2026),
            selectInput("target_sector_sr", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
          ),
          column(
            width = 9,
            h4("Summary Table Placeholder"),
            tableOutput("source_reduction_summary_table"),
            br(),
            h4("Plot Placeholder"),
            plotOutput("source_reduction_plot")
          )
        )
      ),
      
      tabPanel(
        "Recycling Rate",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_rr", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("target_year_rr", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_rr", "Implement Year:", choices = 2026:2050, selected = 2026),
            selectInput("target_sector_rr", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
          ),
          column(
            width = 9,
            h4("Summary Table Placeholder"),
            tableOutput("recycling_rate_summary_table"),
            br(),
            h4("Plot Placeholder"),
            plotOutput("recycling_rate_plot")
          )
        )
      ),
      
      tabPanel(
        "Recycled Content",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_rc", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("target_year_rc", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_rc", "Implement Year:", choices = 2026:2050, selected = 2026),
            selectInput("target_sector_rc", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
          ),
          column(
            width = 9,
            h4("Summary Table Placeholder"),
            tableOutput("recycled_content_summary_table"),
            br(),
            h4("Plot Placeholder"),
            plotOutput("recycled_content_plot")
          )
        )
      )
    )
  ),
  

# SB 54 Policy ------------------------------------------------------------


nav_panel(
  "SB54",
  br(),
  fluidRow(
    column(
      width = 3,
      selectInput(
        "implement_year_54",
        "Implement Year:",
        choices = 2025:2050,
        selected = 2025
      )
    ),
    column(
      width = 9,
      h4("SB 54 Information"), 
      h6("SB54 Text placeholder RR & SR" ),
      h4("Summary Table"),
      tableOutput("sb54_summary_table"),
      br(),
      h4("Plot"),
      plotOutput("sb54_plot")
    ) # END outputs
  ) # END fluid row
), 
  
  
  # Combined Policy ---------------------------------------------------------

nav_panel(
  "Combined Policy",
  br(),
  
  fluidRow(
    # ---- LEFT: accordion sidebar ----
    column(
      width = 3,
      accordion(
        id = "combined_policy_accordion",
        open = TRUE,
        
        accordion_panel(
          "Source Reduction",
          numericInput("target_sr", "Rate (%):", value = 0, min = 0, max = 100),
          selectInput("baseline_year_sr", "Baseline Year:", choices = 1950:2025, selected = 2023),
          selectInput("target_year_sr", "Target Year:", choices = 2026:2050, selected = 2030),
          selectInput("implement_year_sr", "Implement Year:", choices = 2026:2050, selected = 2026),
          selectInput("target_sector_sr", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
        ),
        
        accordion_panel(
          "Recycling Rate",
          numericInput("target_rr", "Rate (%):", value = 0, min = 0, max = 100),
          selectInput("target_year_rr", "Target Year:", choices = 2026:2050, selected = 2030),
          selectInput("implement_year_rr", "Implement Year:", choices = 2026:2050, selected = 2026),
          selectInput("target_sector_rr", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
        ),
        
        accordion_panel(
          "Recycled Content",
          numericInput("target_rc", "Rate (%):", value = 0, min = 0, max = 100),
          selectInput("target_year_rc", "Target Year:", choices = 2026:2050, selected = 2030),
          selectInput("implement_year_rc", "Implement Year:", choices = 2026:2050, selected = 2026),
          selectInput("target_sector_rc", "Target Sector:", choices = c("Packaging" = "pack"), selected = "pack")
        )
      )
    ), # END left column
    
    # ---- RIGHT: table + plot ----
    column(
      width = 9,
      h4("Summary Table"),
      tableOutput("combined_policy_summary_table"),
      br(),
      h4("Plot"),
      plotOutput("combined_policy_plot")
    )
  ) # END fluidRow
), # END nav_panel
  # ---------------- Comparison ----------------
  nav_panel(
    "Comparison",
    br(),
    h4("Summary Table"),
    tableOutput("comparison_summary_table"),
    br(),
    h4("Plot"),
    plotOutput("comparison_plot")
  )
)