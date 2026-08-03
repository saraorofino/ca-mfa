
# load packages -----------------------------------------------------------



library(shiny)

# UI

ui <- fluidPage(
  
  titlePanel("Plastic Policy Impacts Model"),


# sidebar: persistent inputs across tabs (outside tabPanel only renders once) -----------------

sidebarLayout(
  sidebarPanel(
    width = 3,
    
    h4("Scenario Inputs"),
    
    selectInput(
      inputId  = "state",
      label    = "State:",
      choices  = c("California" = "CA", "New York" = "NY", "Texas" = "TX")),
      selected = "CA"
    )
      
      # Additional shared inputs placeholder (e.g. choose recycling rate CA or National Average)
      # They will also persist across tabs.
    ),

# main panel: top-level tabs ----------------------------------------------

   
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        # ---------------- Overview ----------------
        tabPanel(
          "Overview",
          br(),
          h4("Summary Table"),
          tableOutput("overview_summary_table"),
          br(),
          h4("Plot"),
          plotOutput("overview_plot")
        ),
        
        # ---------------- Individual Policy (with sub-tabs) ----------------
        tabPanel(
          "Individual Policy",
          br(),
          tabsetPanel(
            id = "individual_policy_tabs",
            
            tabPanel(
              "Source Reduction",
              br(),
              h4("Summary Table"),
              tableOutput("source_reduction_summary_table"),
              br(),
              h4("Plot"),
              plotOutput("source_reduction_plot")
            ),
            
            tabPanel(
              "Recycling Rate",
              br(),
              h4("Summary Table"),
              tableOutput("recycling_rate_summary_table"),
              br(),
              h4("Plot"),
              plotOutput("recycling_rate_plot")
            ),
            
            tabPanel(
              "Recycled Content",
              br(),
              h4("Summary Table"),
              tableOutput("recycled_content_summary_table"),
              br(),
              h4("Plot"),
              plotOutput("recycled_content_plot")
            )
          )
        ),
        

# SB54 tab  ---------------------------------------------------------------

        tabPanel(
          "SB54",
          br(),
          h4("Summary Table"),
          tableOutput("sb54_summary_table"),
          br(),
          h4("Plot"),
          plotOutput("sb54_plot")
        ),


# Combined Policy Tab -----------------------------------------------------

        tabPanel(
          "Combined Policy",
          br(),
          h4("Summary Table"),
          tableOutput("combined_policy_summary_table"),
          br(),
          h4("Plot"),
          plotOutput("combined_policy_plot")
        ),
        

# Comparison Tab ----------------------------------------------------------

        tabPanel(
          "Comparison",
          br(),
          h4("Summary Table"),
          tableOutput("comparison_summary_table"),
          br(),
          h4("Plot"),
          plotOutput("comparison_plot")
        )
      )
    )
  )
)