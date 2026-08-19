

server <- function(input, output, session) {
  

# Pop Up Instructions -----------------------------------------------------
  showModal(
    modalDialog(
      title = "Welcome to the Plastic Policy Model!",
      p(
        tags$strong("Step 1."), "Choose your state & sector.",
        br(),
        tags$strong ("Step 2."), "Enter your target rates & years for Individual Policies, CA SB 54 or Combined Polices.",
        br(),
       tags$strong ("Step 3."), "Visualize your selections side-by-side in the Comparison tab."
      ),
      easyClose = TRUE,
      footer = modalButton("Continue")
    )
  )
  

# Create BAU from State Input ---------------------------------------------
  state_abbr <- reactive({
    input$state
  })
  
  consum_bau <- reactive({
    
    results <- calc_consum_bau(
      bea_to_plastic = bea_to_plastic,
      state_abbr = state_abbr(),
      consumption_element = "Consumption_Complete",
      scaled_na_consumption = scaled_na_consumption,
      n_iterations = 4
    )
    results
  })
  
  
  
  # --- helper: dummy placeholder table ---------------------------------
  placeholder_table <- function(label) {
    data.frame(
      Impact  = c("Avoided Primary Production", "Avoided Greenhouse Gases (Co2e)", "Total Production"),
      value = c(NA, NA, NA)
    )
  }
  
  # --- helper: dummy placeholder plot -----------------------------------
  placeholder_plot <- function(label) {
    plot(
      1, type = "n", xlab = "", ylab = "",
      main = paste("Placeholder plot -", label)
    )
    text(1, 1, "No data yet", col = "gray50")
  }


# Incineration Static Data Input (future reactive) ------------------------

  incineration <- reactive({
    if (state_abbr() == "CA") {
      ca_incineration
    } else {
      warning(paste0("No state-specific incineration data for '", state_abbr(), "'. Using national average."))
      avg_incineration
    }
  })
  
   # Run Bau Model Results ---------------------------------------------------
   
   
  bau_results <- reactive({
    
    results <- run_bau(
      consum_bau(),
      incineration = ca_incineration,
      emission_factors = emission_factors,
      lifetimes = lifetimes,
      bau_rr_sect = ca_rr
    )
    
    results
  })

### could make bau_rr_sect reactive in the future for other sector and state recycling rates
   

# Welcome -----------------------------------------------------------------

  output$overview_summary_table <- renderTable({
    placeholder_table("Overview")
  })
  output$bau_overview_plot <- renderPlot({
    df <- consum_bau()
    
    
    consum_bau_time_plot <- ggplot(df,
                                   aes( x = year,
                                        y = mt_plastic_bau,
                                        fill = sector)) +
      geom_area(data = filter(df)) +
      labs(x = "Year",
           y = "Plastic Consumed Per Year (Million Metric Tons)",
           fill = "Sector") +
      theme_bw() +
      scale_fill_manual(values = c(
        pack = "#1B5E3C",
        buil = "#9ACD32",
        tran = "#7FC7C4",
        heal = "#A8D8B9",
        comm = "#5B9BD5",
        elec = "#2E75B6",
        hous = "#1F4E8C",
        mach = "#0D2C5C",
        text = "#C87137",
        othe = "#C4B8A8",
        agri = "#F5C071"
      ),
      labels = sector_labels)
    consum_bau_time_plot
  })
  
  # ---------------- Individual Policy: Source Reduction ----------------
 
  
sr_results <- eventReactive(input$run_sr, {
    params <- tibble(
      policy_rate    = input$target_sr / 100,   # converting from percent
      implement_year = as.numeric(input$implement_year_sr),
      target_year    = as.numeric(input$target_year_sr),
      baseline_year  = as.numeric(input$baseline_year_sr),
      target_sector  = input$sector        
    )
    run_policy_sr(params, bau_results = bau_results(), incineration = incineration(), consum_bau = consum_bau())
  })
  
  output$source_reduction_summary_table <- renderTable({
    res <- sr_results()
    tibble(
      Impact = c("Total Consumption (MT)", "Avoided Primary Production (MT)", "Avoided GHG (MT CO2e)"),
      value  = c(res$total_consumption_sr, res$total_avoid_prod_sr, res$total_ghg_diff_sr)
    )
  })
  
 
  
  
  # ---------------- Individual Policy: Recycling Rate ----------------
  
  
  
  
  rr_results <- eventReactive(input$run_rr, {
    params_rr <- tibble(
      target_rr         = input$target_rr /100, #converting from percent
      implement_year_rr = as.numeric(input$implement_year_rr),
      target_year_rr   = as.numeric(input$target_year_rr),
      #baseline_year_rr  = as.numeric(input$baseline_year), # only for sr
      target_sector_rr  = input$sector
    )
    run_policy_rr(params_rr, bau_results = bau_results(), incineration = incineration(), consum_bau = consum_bau())
   })
  
  output$recycling_rate_summary_table <- renderTable({
    rr_res <- rr_results()
    tibble(
      Impact = c("Total Consumption (MT)", "Avoided Primary Production (MT)", "Avoided GHG (MT CO2e)"),
      value  = c(rr_res$total_consumption_rr, rr_res$total_avoid_prod_rr, rr_res$total_ghg_diff_rr)
    )
  })
  
  output$recycling_rate_plot <- renderPlot({
    placeholder_plot("Recycling Rate")
  })
  
  
  
  # ---------------- Individual Policy: Recycled Content ----------------
 

  
  rc_results <- eventReactive(input$run_rc, {
    params_rc <- tibble(
      target_rc         = input$target_rc / 100, # converting from percent
      implement_year_rc = as.numeric(input$implement_year_rc),
      target_year_rc    = as.numeric(input$target_year_rc),
      target_sector_rc  = input$sector
    )
    
    run_policy_rc(params_rc, bau_results(), incineration(), consum_bau= consum_bau())
  })
  
  output$recycled_content_summary_table <- renderTable({
    rc_res <- rc_results()
    
    tibble(
      Impact = c(
        "Total Consumption (MT)",
        "Avoided Primary Production (MT)",
        "Avoided GHG (MT CO2e)"
      ),
      value = c(
        rc_res$total_consumption_rc,
        rc_res$total_avoid_prod_rc,
        rc_res$total_ghg_diff_rc
      )
    )
  })
  
  output$recycled_content_plot <- renderPlot({
    placeholder_plot("Recycled Content")
  })
  
  
  
  # ---------------- SB54 ----------------
  output$sb54_summary_table <- renderTable({
    placeholder_table("SB54")
  })
  output$sb54_plot <- renderPlot({
    placeholder_plot("SB54")
  })
  
  sb54_results <- eventReactive(input$run_sb54, {
    params_sb54 <- tibble(
      implement_year_54 = as.numeric(input$implement_year_54),
      target_year       = as.numeric(input$target_year_54)
    )
    
    run_policy_sb54(params_sb54, bau_results(), incineration(),consum_bau = consum_bau())
  })
  
  output$sb54_summary_table <- renderTable({
    sb54_res <- sb54_results()
    
    tibble(
      Impact = c(
        "Total Consumption (MT)",
        "Avoided Primary Production (MT)",
        "Avoided GHG (MT CO2e)"
      ),
      value = c(
        sb54_res$total_consumption_sb54,
        sb54_res$total_avoid_prod_sb54,
        sb54_res$total_ghg_diff_sb54
      )
    )
  })
  
  
  
  # ---------------- Combined Policy ----------------
  output$combined_policy_summary_table <- renderTable({
    placeholder_table("Combined Policy")
  })
 
  
  comp_results <- eventReactive(input$run_comp, {
    params_comp <- tibble(
      # sr — renamed to match what run_policy_comp() expects
      policy_rate_sr    = input$target_sr_comp / 100,
      baseline_year_sr  = as.numeric(input$baseline_year_sr_comp),
      target_year_sr    = as.numeric(input$target_year_sr_comp),
      implement_year_sr = as.numeric(input$implement_year_sr_comp),
      target_sector_sr  = input$sector,
      
      # rr
      policy_rate_rr    = input$target_rr_comp / 100,
      target_year_rr    = as.numeric(input$target_year_rr_comp),
      implement_year_rr = as.numeric(input$implement_year_rr_comp),
      target_sector_rr  = input$sector,
      
      # rc
      policy_rate_rc    = input$target_rc_comp / 100,
      target_year_rc    = as.numeric(input$target_year_rc_comp),
      implement_year_rc = as.numeric(input$implement_year_rc_comp),
      target_sector_rc  = input$sector,
      baseline_rc       = 0,     # not exposed in UI yet — hardcoded default
      is_scrap_consump  = 0.5    # not exposed in UI yet — hardcoded default
    )
    
    run_policy_comp(params_comp, bau_results(), incineration(), consum_bau = consum_bau())
  })
  
  output$combined_policy_summary_table <- renderTable({
    comp_res <- comp_results()
    
    tibble(
      Impact = c(
        "Total Consumption (MT)",
        "Avoided Primary Production (MT)",
        "Avoided GHG (MT CO2e)"
      ),
      value = c(
        comp_res$total_consumption_comp,
        comp_res$total_avoid_prod_comp,
        comp_res$total_ghg_diff_comp
      )
    )
  })
  
  # ---------------- Comparison ----------------
  output$comparison_summary_table <- renderTable({
    placeholder_table("Comparison")
  })
  output$comparison_plot <- renderPlot({
    placeholder_plot("Comparison")
  })
}