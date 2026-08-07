ui <- fluidPage(
  titlePanel("USEEIO State Model Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State", choices = available_states),
      actionButton("load", "Load State Model"),
      br(), br(),
      uiOutput("status"),
      br(),
      uiOutput("year_ui")
    ),
    mainPanel(
      verbatimTextOutput("structure"),
      tableOutput("preview")
    )
  )
)