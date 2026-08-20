

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
  )
  ),
  

# Welcome ----------------------------------------------------------------

  nav_panel(
    "Welcome",
    fillable = FALSE,
    h4("What it does"),
    h6("This is the first state-level, time-dependent material flow analysis (MFA) of plastics. It draws upon the EPA environmentally extended input out state data sets, converting plastic dollar value into tons."),
    h6(("The model assesses three policy strategies, both separately and combined:"),strong("source reduction, recycling rate and recycled content mandates.")),
    br(), 
    h6("The analysis quantifies plastic consumption, waste generation, and end-of-life management across all major use sectors and evaluates the projected impacts of key policy interventions, including California’s landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act, Senate Bill 54 (SB 54)."),
    br(), 
    
h4("How to use it"),
h6(
  strong("Welcome"), " — see business-as-usual projected plastic consumption by selecting your state in the side panel.",
  br(), br(),
  strong("Explore Solutions"), " — set the policy targets (implement year, target year, rate), for individual mandates or combined mandates for interactive policy effects.",
  br(), br(),
  strong("CA SB54"), " — learn about California's landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act and model policy delay impacts.",
  br(), br(),
  strong("Compare Solutions"), " — see your solutions side-by-side by selecting two of your previously modeled solutions or CA SB54.",
  br(), br(),
  strong("About"), " — where the model comes from, who worked on it, what sources were referenced and which assumptions remain uncertain."
),
br(), br(),

h4("Why it matters"),

h6(
  withSpinner(uiOutput("sum_bau", inline = TRUE), type = 1)),

br(), h4(
  uiOutput("state_abbr", inline = TRUE),("Projected Plastic Consumption By Sector 1950-2050:")
), br(), withSpinner(plotOutput("bau_overview_plot", height = "500px"), type = 1)
  ), 

# The Problem  ------------------------------------------------------------

nav_panel(
  "The Problem",
  br(), 
  h4("Plastic Consumption Crisis")), 
  
  # ---------------- Explore Solutions (with sub-tabs) ----------------
  nav_panel(
    "Explore Solutions",
    br(),
    tabsetPanel(
      id = "individual_policy_tabs",
      
 ## Source Reduction--------------------------------------------------------

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
            actionButton("run_sr", "Model Policy", class = "btn-primary") # END Run Button
          ), 
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            
            withSpinner(tableOutput("source_reduction_summary_table"), type = 1),
            
            
            
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
            actionButton("run_rr", "Model Policy", class = "btn-primary") # END Run Button
          ),
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            
            withSpinner(tableOutput("recycling_rate_summary_table"), type = 1),
        
            br(),
            h4("Plastic End-of-Life Projections Compared to Business-as-Usual"),
            h6("Recycling Rate mandates alone do not change consumption levels."),
            plotOutput("rr_eol_plot")
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
            actionButton("run_rc", "Model Policy", class = "btn-primary") 
          ),
          column(
            width = 9,
            h4("Projected Policy Impacts Compared to Business-As-Usual"),
            h6("Cumulative Results from Implement Year to 2050"),
            withSpinner(tableOutput("recycled_content_summary_table"), type = 1),
            br(),
            h4("Plastic End-of-Life Projections Compared to Business-as-Usual"),
            h6("Recycled Content mandates alone do not change consumption levels."),
            plotOutput("recycled_content_plot")
          )
        )
      ),
   

# Combined policy ---------------------------------------------------------

#moved to within explore solutions

tabPanel(
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
  br(),
  h4("Plot"),
  withSpinner(plotOutput("comp_eol_plot"))
        )  # closes Combined Policy tabPanel()
    )    # closes tabsetPanel()
  ), # END nav_panel
  

# SB 54 Policy ------------------------------------------------------------


nav_panel(
  "CA SB54",
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
      actionButton("run_sb54", "Model Policy", class = "btn-primary") 
    ),
    column(
      width = 9,
      h4("SB 54 Information"), 
      h6("SB54 Text placeholder RR & SR" ),
      h4("SB54 Impacts Compared to Business-As-Usual Due to Delayed Targets"),
      h6("Cumulative Results from Implement Year to 2050"),
      withSpinner(tableOutput("sb54_summary_table"), type = 1),
      br(),
      h4("Plot"),
      withSpinner(plotOutput("sb54_eol_plot"))
    ) # END outputs
  ) # END fluid row
), 



# Compare Solutions -------------------------------------------------------
  nav_panel(
    "Compare Solutions",
    br(),
    br(), 
    actionButton("run_both", "Model Policy", class = "btn-primary"), 
    
    h4("Summary Table"),
    withSpinner(tableOutput("comparison_summary_table"), type = 1),
    br(),
    h4("Plot"),
    plotOutput("comparison_plot")
  ),

# About  ------------------------------------------------------------------
nav_panel(
  "About",
  br(), 
  br(),
  h6("About filler text")
)
)