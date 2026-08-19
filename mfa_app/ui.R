
library(shiny)
library(bslib)

ui <- page_navbar(

# Font Selection ----------------------------------------------------------

  tags$link(
    rel = "stylesheet",
    href = "https://fonts.googleapis.com/css2?family=Baskervville:wght@400;500;600;700&family=Epilogue:wght@400;500;600;700&display=swap"
  ),
  
  tags$style(HTML("
    /* Headers and titles */
    h1, h2, h3, h4, h5, h6 {
      font-family: 'Baskervville', sans-serif;
    }

    /* Body text */
    body {
      font-family: 'Epilogue', serif;
    }

    /* Shiny inputs, buttons, etc. */
    button,
    input,
    select,
    textarea,
    .form-control,
    .btn,
    .selectize-input,
    .selectize-dropdown {
      font-family: 'Epilogue', serif;
    }
    ")),
    

# add photo background  ---------------------------------------------------

 tags$style(HTML("
                  .pollution-card {
                    background-image:
                      linear-gradient(
                        rgba(0, 0, 0, 0.45),
                        rgba(0, 0, 0, 0.45)
                      ),
                    url('shutterstock_645210340.jpg');
                    
                    background-size: cover;
                    background-position: center;
                    background-repeat: no-repeat;
                    
                    height: 900px;
                    padding: 25px;
                    border-radius: 12px;
                    
                    color: white;
                    
                    display: flex;
                    align-items: flex-start; # change to flex-end to move to bottom
                  }
                  
                  .pollution-card h2 {
                    color: white;
                    margin: 0;
                  }
                  ")),
                  
  title = "Plastic Policy Impact Model",
  id = "main_tabs",
  theme = bs_theme(version = 5),
  fillable = FALSE,

# Side Panel: State Inputs ------------------------------------------------

  sidebar = sidebar(
    width = 400,
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
    ), # END state input
    
    selectInput(
      inputId = "sector",
      label = "Sector:",
      choices = c("Packaging" = "pack")
    ), # END sector input
    br(), 
    br(),
    div(
      class = "pollution-card",

    
    h2("Plastic pollution has reached a crisis point –",
      tags$strong("Policy is Essential to protect human health.")
      )
    
    
    
    # Additional shared inputs placeholder (e.g. choose recycling rate CA or National Average, incineration)
    # They will also persist across tabs.
  ),),
  

# Welcome ----------------------------------------------------------------

  nav_panel(
    "Welcome",
    fillable = FALSE,
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
    
    withSpinner(plotOutput("bau_overview_plot", height = "500px"), type = 1)
  ),
  
  # ---------------- Individual Policy (with sub-tabs) ----------------
  nav_panel(
    "Individual Policy",
    br(),
    tabsetPanel(
      id = "individual_policy_tabs",
      
 # Source Reduction--------------------------------------------------------

      tabPanel(
        "Source Reduction",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput(
              "target_sr",
              "Rate (%):",
              value = 0,
              min = 0,
              max = 100
            ),
            selectInput(
              "baseline_year_sr",
              "Baseline Year:",
              choices = 1950:2025,
              selected = 2023
            ),
            selectInput(
              "target_year_sr",
              "Target Year:",
              choices = 2026:2050,
              selected = 2030
            ),
            selectInput(
              "implement_year_sr",
              "Implement Year:",
              choices = 2026:2050,
              selected = 2026
            ),

            br(), 
            actionButton("run_sr", "Model Policy", class = "btn-primary"), # END Run Button
          ), 
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            
            withSpinner(tableOutput("source_reduction_summary_table"), type = 1),
            
            
            tableOutput("source_reduction_summary_table"),
            br(),
            h4("Plot Placeholder"),
            plotOutput("source_reduction_plot")
          )
        )
      ), 
      

# Recycling Rate ----------------------------------------------------------

      tabPanel(
        "Recycling Rate",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_rr", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("target_year_rr", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_rr", "Implement Year:", choices = 2026:2050, selected = 2026),
            br(), 
            actionButton("run_rr", "Model Policy", class = "btn-primary"), # END Run Button
          ),
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            
            withSpinner(tableOutput("recycling_rate_summary_table"), type = 1),
        
            tableOutput("recycling_rate_summary_table"),
            br(),
            h4("Plastic End-of-Life Projections Compared to Business-as-Usual"),
            h6("Recycling Rate mandates alone do not change consumption levels."),
            plotOutput("recycling_rate_plot")
          )
        )
      ),

# Recycled Content --------------------------------------------------------


      tabPanel(
        "Recycled Content",
        br(),
        fluidRow(
          column(
            width = 3,
            numericInput("target_rc", "Rate (%):", value = 0, min = 0, max = 100),
            selectInput("target_year_rc", "Target Year:", choices = 2026:2050, selected = 2030),
            selectInput("implement_year_rc", "Implement Year:", choices = 2026:2050, selected = 2026),
            br(), 
            actionButton("run_rc", "Model Policy", class = "btn-primary"), 
          ),
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            withSpinner(tableOutput("recycled_content_summary_table"), type = 1),
            tableOutput("recycled_content_summary_table"),
            br(),
            h4("Plastic End-of-Life Projections Compared to Business-as-Usual"),
            h6("Recycled Content mandates alone do not change consumption levels."),
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
        selected = 2024
      ),
      selectInput("target_year_54", "Target Year:", choices = 2025:2050, selected = 2032),
      br(), 
      actionButton("run_sb54", "Model Policy", class = "btn-primary"), 
    ),
    column(
      width = 9,
      h4("SB 54 Information"), 
      h6("SB54 Text placeholder RR & SR" ),
      h4("SB54 Impacts Compared to Business-As-Usual Due to Delayed Targets"),
      h6("Cumulative Results from Implement Year to 2050"),
      withSpinner(tableOutput("sb54_summary_table"), type = 1),
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
  
  accordion(
    id = "combined_policy_accordion",
    open = TRUE,  # or c("Source Reduction", "Recycling Rate") to open specific ones by default
    
    accordion_panel(
      "Source Reduction",
      fluidRow(
        column(2, numericInput("target_sr_comp", "Rate (%):", value = 0, min = 0, max = 100)),
        column(2, selectInput("baseline_year_sr_comp", "Baseline Year:", choices = 1950:2025, selected = 2023)),
        column(2, selectInput("target_year_sr_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
        column(2, selectInput("implement_year_sr_comp", "Implement Year:", choices = 2026:2050, selected = 2026))
      )
    ),
    
    accordion_panel(
      "Recycling Rate",
      fluidRow(
        column(3, numericInput("target_rr_comp", "Rate (%):", value = 0, min = 0, max = 100)),
        column(3, selectInput("target_year_rr_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
        column(3, selectInput("implement_year_rr_comp", "Implement Year:", choices = 2026:2050, selected = 2026)),
      )
    ),
    
    accordion_panel(
      "Recycled Content",
      fluidRow(
        column(3, numericInput("target_rc_comp", "Rate (%):", value = 0, min = 0, max = 100)),
        column(3, selectInput("target_year_rc_comp", "Target Year:", choices = 2026:2050, selected = 2030)),
        column(3, selectInput("implement_year_rc_comp", "Implement Year:", choices = 2026:2050, selected = 2026)),
      )
    ),
    br(), 
    actionButton("run_comp", "Model Policy", class = "btn-primary"), ),
  
  hr(),
  
  h4("Projected Policy Impacts Compared to Business-As-Usual"),
  h6("Cumulative Results from Implement Year to 2050"),
  withSpinner(tableOutput("combined_policy_summary_table"), type = 1),
  tableOutput("combined_policy_summary_table"),
  br(),
  h4("Plot"),
  plotOutput("combined_policy_plot")
), # END nav_panel

  # ---------------- Comparison ----------------
  nav_panel(
    "Comparison",
    br(),
    br(), 
    actionButton("run_both", "Model Policy", class = "btn-primary"), 
    
    h4("Summary Table"),
    withSpinner(tableOutput("comparison_summary_table"), type = 1),
    tableOutput("comparison_summary_table"),
    br(),
    h4("Plot"),
    plotOutput("comparison_plot")
  )
)