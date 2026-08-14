

# server instructions -----------------------------------------------------
server <- function(input, output) {}
library(shiny)

# ==============================================================
# SERVER
# input$state (from the sidebar) is available to every render*
# function and only needs to be read, not re-initialized.


server <- function(input, output, session) {
  
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
  

  # background: running the run_bau function to get bau_results output------

   bau_results <- run_bau(consum_bau) 
    
  


# -----removing state abbrev from incineration dataframe --------------------

   relabel_incineration <- function(state_abbr) {
    
     df_name <- paste0(state_abbr, "_incineration")
     
     if (!exists(df_name)) {
       stop(
         "Object '",
         df_name,
         "' not found. Run download_rds_state_model('",
         state_abbr,
         "') first."
       )
     }
     
     incineration <- get(df_name)
     
     return(incineration)
     
   }
   
   incineration <- relabel_incineration("ca")
   
   
  # ---------------- Overview ----------------
  output$overview_summary_table <- renderTable({
    placeholder_table("Overview")
  })
  output$bau_overview_plot <- renderPlot({
    sector_labels <- c(
      pack = "Packaging",
      buil = "Building/Construction",
      tran = "Transportation",
      heal = "Healthcare",
      comm = "Commercial/Institutional",
      elec = "Electrical/Electronic",
      hous = "Household/Leisure/Sports",
      mach = "Machinery",
      text = "Textiles",
      othe = "Other",
      agri = "Agriculture"
    )
    consum_bau_time_plot <- ggplot(consum_bau,
                                   aes( x = year,
                                        y = mt_plastic_bau,
                                        fill = sector)) +
      geom_area(data = filter(consum_bau, sector != 'all_sec')) +
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
    (consum_bau_time_plot)
  })
  
  # ---------------- Individual Policy: Source Reduction ----------------
 
  
sr_results <- reactive({
    params <- tibble(
      policy_rate    = input$target_sr / 100,   # converting from percent
      implement_year = as.numeric(input$implement_year_sr),
      target_year    = as.numeric(input$target_year_sr),
      baseline_year  = as.numeric(input$baseline_year_sr),
      target_sector  = input$target_sector_sr,        
    )
    run_policy_sr(params, bau_results)
  })
  
  output$source_reduction_summary_table <- renderTable({
    res <- sr_results()
    tibble(
      Impact = c("Total Consumption (MT)", "Avoided Primary Production (MT)", "Avoided GHG (MT CO2e)"),
      value  = c(res$total_consumption_sr, res$total_avoid_prod_sr, res$total_avoid_ghg_sr)
    )
  })
  
 
  
  
  
  # ---------------- Individual Policy: Recycling Rate ----------------
  
  
  
  
  rr_results <- reactive({
    params_rr <- tibble(
      target_rr         = input$target_rr /100, #converting from percent
      implement_year_rr = as.numeric(input$implement_year_rr),
      target_year_rr   = as.numeric(input$target_year_rr),
      #baseline_year_rr  = as.numeric(input$baseline_year), # only for sr
      target_sector_rr  = input$target_sector_rr
    )
    run_policy_rr(params_rr, bau_results)
   })
  
  output$recycling_rate_summary_table <- renderTable({
    rr_res <- rr_results()
    tibble(
      Impact = c("Total Consumption (MT)", "Avoided Primary Production (MT)", "Avoided GHG (MT CO2e)"),
      value  = c(rr_res$total_consumption_rr, rr_res$total_avoid_prod_rr, rr_res$total_avoid_ghg_rr)
    )
  })
  
  output$recycling_rate_plot <- renderPlot({
    placeholder_plot("Recycling Rate")
  })
  
  
  
  # ---------------- Individual Policy: Recycled Content ----------------
  output$recycled_content_summary_table <- renderTable({
    placeholder_table("Recycled Content")
  })
  output$recycled_content_plot <- renderPlot({
    placeholder_plot("Recycled Content")
  })
  
  rc_results <- reactive({
    params_rc <- tibble(
      target_rc         = input$target_rc / 100, # converting from percent
      implement_year_rc = as.numeric(input$implement_year_rc),
      target_year_rc    = as.numeric(input$target_year_rc),
      target_sector_rc  = input$target_sector_rc
    )
    
    run_policy_rc(params_rc, bau_results)
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
        rc_res$total_avoid_ghg_rc
      )
    )
  })
  
  
  
  
  # ---------------- SB54 ----------------
  output$sb54_summary_table <- renderTable({
    placeholder_table("SB54")
  })
  output$sb54_plot <- renderPlot({
    placeholder_plot("SB54")
  })
  
  sb54_results <- reactive({
    params_sb54 <- tibble(
      implement_year_54 = as.numeric(input$implement_year_54),
      target_year       = as.numeric(input$target_year_54)
    )
    
    run_policy_sb54(params_sb54, bau_results)
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
        sb54_res$total_avoid_ghg_sb54
      )
    )
  })
  
  
  
  # ---------------- Combined Policy ----------------
  output$combined_policy_summary_table <- renderTable({
    placeholder_table("Combined Policy")
  })
  output$combined_policy_plot <- renderPlot({
    placeholder_plot("Combined Policy")
  })
  
  # ---------------- Comparison ----------------
  output$comparison_summary_table <- renderTable({
    placeholder_table("Comparison")
  })
  output$comparison_plot <- renderPlot({
    placeholder_plot("Comparison")
  })
}