server <- function(input, output, session) {
  
  # session-level cache: state -> list of year -> model
  cache <- reactiveValues(data = list())
  
  state_models <- eventReactive(input$load, {
    st <- input$state
    
    if (!is.null(cache$data[[st]])) {
      return(cache$data[[st]])
    }
    
    result <- NULL
    withProgress(message = paste("Loading", st, "models (2012–2020)..."), value = 0.2, {
      result <- tryCatch({
        load_state_models(st, rds_files, base_url)
      }, error = function(e) {
        showNotification(paste("Failed to load:", e$message), type = "error")
        NULL
      })
      incProgress(0.8)
    })
    
    cache$data[[st]] <- result
    result
  })
  
  output$status <- renderUI({
    req(input$load)
    mods <- state_models()
    if (is.null(mods)) {
      tags$p(style = "color:red;", "No models loaded.")
    } else {
      tags$p(style = "color:green;", paste0("Loaded ", input$state, ": ",
                                            length(mods), " years (",
                                            paste(names(mods), collapse = ", "), ")"))
    }
  })
  
}
