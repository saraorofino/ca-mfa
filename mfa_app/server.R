

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
  
  # ---------------- Overview ----------------
  output$overview_summary_table <- renderTable({
    placeholder_table("Overview")
  })
  output$overview_plot <- renderPlot({
    placeholder_plot("Overview")
  })
  
  # ---------------- Individual Policy: Source Reduction ----------------
  output$source_reduction_summary_table <- renderTable({
    placeholder_table("Source Reduction")
  })
  output$source_reduction_plot <- renderPlot({
    placeholder_plot("Source Reduction")
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