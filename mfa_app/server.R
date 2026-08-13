

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
  

  # background --------------------------------------------------------------

  # bau_results <- run_bau(consum_bau) --- moving this into global?
    
  
  
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
      policy_rate    = input$target_sr / 100,   # converting to a percent
      implement_year = as.numeric(input$implement_year_sr),
      target_year    = as.numeric(input$target_year_sr),
      baseline_year  = as.numeric(input$baseline_year_sr),
      target_sector  = input$target_sector_sr,        
    )
    run_policy_sr(params)
  })
  
  output$source_reduction_summary_table <- renderTable({
    res <- sr_results()
    tibble(
      Impact = c("Total Consumption (MT)", "Avoided Primary Production (MT)", "Avoided GHG (MT CO2e)"),
      value  = c(res$total_consumption_sr, res$total_avoid_prod_sr, res$total_avoid_ghg_sr)
    )
  })
  
  output$source_reduction_plot <- renderPlot({
    res <- sr_result()
    plot(res$consum_sr_data$year, res$consum_sr_data$mt_plastic_sr,
         type = "l", xlab = "Year", ylab = "MT Plastic",
         main = "Source Reduction: Consumption Over Time")
  })
  
  
  
  # ---------------- Individual Policy: Recycling Rate ----------------
  output$recycling_rate_summary_table <- renderTable({
    placeholder_table("Recycling Rate")
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
  
  # ---------------- SB54 ----------------
  output$sb54_summary_table <- renderTable({
    placeholder_table("SB54")
  })
  output$sb54_plot <- renderPlot({
    placeholder_plot("SB54")
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