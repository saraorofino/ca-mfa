

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
      choices = state_choices, 
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
    h2("Why it matters"),
    h5("Advocacy groups and law makers face critical data-gaps when seeking policy solutions to curb plastic pollution. The Plastic Policy Impact Model seeks to:"),
    h5(tags$p(style = "margin-left: 20px;", "1) enable regulators to make informed, science-backed decisions; and"),
       tags$p(style = "margin-left: 20px;", "2) to raise collective awareness that a world with less plastic is both possible and essential for the well-being of people and the planet.")),
    h5(
      "Through interactive policy levers, targets and timelines, users can visualize how policy can change the trajectory of plastic consumption and the cost of inaction. The plastic crisis emerged within a single generation, with large-scale production and use only dating back to ~1950.",
      tags$sup("1"),
      " It can be reversed just as quickly if we take bold action now."
    ),
br(),
h2("How to use it"),
h5(
  strong("Welcome"), " —  select your state in the side panel to get started.",
  br(), br(),
  strong("The Problem"), " — understand the urgency of the plastic crisis, including your states business-as-usual projections.",
  br(), br(),
  strong("Explore Solutions"), " — set the policy targets (implement year, target year, rate), for individual mandates or combined mandates for interactive policy effects.",
  br(), br(),
  strong("CA SB54"), " — learn about California's landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act and model policy delay impacts.",
  br(), br(),
  strong("Compare Solutions"), " — see your solutions side-by-side by selecting two of your previously modeled solutions or CA SB54.",
  br(), br(),
  strong("About"), " — where the model comes from, who worked on it, what sources were referenced and which assumptions remain uncertain."
),
br(), 

h2("What it does"),
h5("This is the first state-level, time-dependent material flow analysis (MFA) of plastics. It draws upon the EPA environmentally extended input out state data sets, converting plastic dollar value into tons.The model assesses three policy strategies, both separately and combined:",strong("source reduction, recycling rate and recycled content mandates.")),
br(), 
h5("The analysis quantifies plastic consumption, waste generation, and end-of-life management across all major use sectors and evaluates the projected impacts of key policy interventions, including California’s landmark Plastic Pollution Prevention and Packaging Producer Responsibility Act, Senate Bill 54 (SB 54)."),

a(href = "https://drive.google.com/file/d/1BCfB1w6JrAlvwVrymREnvxWwfP6wIpkp/view?usp=sharing", target = "_blank", "For detailed methodology, read the full report here.")),

# The Problem  ------------------------------------------------------------

nav_panel(
  "The Problem",
  h2(class= "text-center",uiOutput("state_sum_intro")), 
  h4(class = "text-center", "Business as usual projections for 2025 to 2050"), br(),
  layout_columns(
    class = "text-center",h4(icon("bottle-water", class = "fa-2xl"), " Total Consumption:",br(), br(), withSpinner(uiOutput("sum_bau", inline = TRUE), type = 1), "million metric tons (Mt).", br(), a(href = "https://www.themeasureofthings.com/results.php?comp=weight&unit=mt&amt=1", target = "_blank", "Contextualize your output here")),
    
    h4(icon("industry",class = "fa-2xl"), " Greenhouse Gas Emissions:", br(), br(), withSpinner(uiOutput("ghg_bau", inline = TRUE), type = 1), "million metric tons of CO2 equivalent", br(),a(href = "https://www.epa.gov/energy/greenhouse-gas-equivalencies-calculator", target = "_blank", "Contextualize your output here"))
  ),
  br(), 
  h5(
    "Plastic production, use, and disposal pose numerous risks to human health, and are associated with increased rates of cardiovascular, pulmonary, renal diseases, and cancers.",
    tags$sup("2,3"),
    " Microplastics have been detected in the air we breathe, the food we eat and the water we drink. Alarmingly, these particles have been found in nearly every part of the human body tested, including blood, lungs, liver, kidneys, placenta and even breast milk.",
    tags$sup("4,5,6,7,8"),
    br(), br(),
    "The adverse effects of plastics and plastic pollution disproportionately affect socioeconomically disadvantaged and marginalized communities.",
    tags$sup("2,11"),
    " Despite growing public concern, plastic production continues to skyrocket. As increased clean energy displaces oil, it is paramount to address the fossil fuel industries intention to dramatically increase plastic production in the coming decades. Find more information on how The Nature Conservancy is fighting plastic pollution", a(href = "https://www.nature.org/en-us/about-us/where-we-work/united-states/california/stories-in-california/stop-plastic-waste/", target = "_blank", "here.") ),
br(),

br(),br(), h2(
  uiOutput("state_full", inline = TRUE),("Plastic Consumption By Sector 1950-2050")
), br(), withSpinner(plotOutput("bau_overview_plot", height = "500px"), type = 1)
), 
  
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