calc_power_series <- function(state_model, n_iterations = 4) {
  
  years <- sort(unique(state_model$year))
  
  results <- vector("list", length(years))
  
  for (yr in years) {
    
    # -----------------------------
    # Get A matrix
    # -----------------------------
    
    A_data <- state_model|> # CHANGE TO BE DYNAMIC WITH STATE MODEL 
      filter(
        year == yr,
        element == "A"
      ) |>
      select(row, col, value)
    
    # -----------------------------
    # Pull out row 326/US-<state> BEFORE matrix math
    # -----------------------------
    
    a_326_df <- A_data |>
      filter(startsWith(row, "326/US")) |>
      select(col, a_326 = value)
    
    # All sector names
    sectors <- union(
      unique(A_data$row),
      unique(A_data$col)
    )
    
    # Build matrix using sector names
    A_matrix <- matrix(
      0,
      nrow = length(sectors),
      ncol = length(sectors),
      dimnames = list(sectors, sectors)
    )
    
    # Fill matrix
    A_matrix[
      cbind(
        match(A_data$row, sectors),
        match(A_data$col, sectors)
      )
    ] <- A_data$value
    
    
    # -----------------------------
    # Get F vector
    # -----------------------------
    
    F_data <- state_model|> ### CHANGE TO BE DYNAMIC
      filter(
        year == yr,
        element == "Consumption_Complete"
      ) |>
      select(row, value, col)
    
    # Create named F vector
    F_vector <- F_data$value
    names(F_vector) <- F_data$row
    

    # -----------------------------
    # Check that all A sectors
    # have corresponding F values
    # -----------------------------
    
    missing_F <- setdiff(sectors, names(F_vector))
    
    if (length(missing_F) > 0) {
      stop(
        "Missing Consumption_Complete values for year ",
        yr,
        ": ",
        paste(missing_F, collapse = ", ")
      )
    }
    
    # -----------------------------
    # Reorder F to EXACTLY match
    # the columns of A
    # -----------------------------
    
    F_vector <- F_vector[colnames(A_matrix)]
    
    # Keep F as a data frame for joining later
    F_df <- data.frame(
      row = names(F_vector),
      final_demand = as.numeric(F_vector)
    )
    
    # Pull out final demand from 326 ------------------------------------------
    
    f_326_value <- F_df |>
      filter(startsWith(row, "326/US")) |>
      pull(final_demand)
    
    
    # -----------------------------
    # Calculate A^n F
    # -----------------------------
    
    results_list <- vector("list", n_iterations)
    
    current_vector <- F_vector
    
    for (i in seq_len(n_iterations)) {
      
      current_vector <- A_matrix %*% current_vector
      
      results_list[[i]] <- as.numeric(current_vector)
    }
    
    
    # -----------------------------
    # Create results data frame
    # -----------------------------
    
    results_df <- as.data.frame(results_list)
    names(results_df) <- paste0(
    "a",
    seq_len(n_iterations),
    "f"
    )
    
    results_df <- results_df |>
      mutate(
        year = yr,
        row = rownames(A_matrix),
        .before = 1
      )
    

# add Leontief values for 326 ------------------------------------------

    L_df <- state_model |>
      filter(year == yr, element == "L", startsWith(row, "326/US")) |>
      select(col, leontief_326 = value)
      
    

    # -----------------------------
    # Bind in f_326 and f_value as columns
    # -----------------------------
    
    results_df <- results_df |>
      left_join(a_326_df, by = c("row" = "col")) |>
      left_join(F_df, by = "row") |>
      left_join(L_df, by = c("row" = "col")) |># NEW
      mutate(
        demand_326 = ifelse(startsWith(row, "326/US"), f_326_value, 0)) 
    
    results[[which(years == yr)]] <- results_df
  }
  
  # Combine years
  bind_rows(results)
}

