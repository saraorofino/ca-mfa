# pulls matrix A times 
# Perform the matrix multiplication in Power Series Sheet
library(readxl)
library(dplyr)

calc_power_series <- function(tab_name, comlete_consumption, n_iterations = 4) {
  
  # Pull in the matrix once (A matrix stays constant across iterations)
  raw_data <- readxl::read_excel(state_model, sheet = tab_name) 
  
  mat_data <- raw_data |> 
    select(-1) |>  # Remove labels in column 1 
    as.matrix()
  mode(mat_data) <- "numeric"
  
  # Starting vector (F)
  f_vector <- complete_consumption[["us_consumption_complete"]] |> as.numeric()
  
  # Dimension check
  if (ncol(mat_data) != length(f_vector)) {
    stop("Dimension mismatch: matrix has ", ncol(mat_data), 
         " columns but vector has ", length(f_vector), " elements.")
  }
  
  # Store each iteration's output in a list
  results_list <- vector("list", n_iterations)
  current_vector <- f_vector
  
  for (i in seq_len(n_iterations)) {
    current_vector <- mat_data %*% current_vector
    results_list[[i]] <- as.numeric(current_vector)
  }
  
  # Combine into a data frame, one column per iteration (af1, af2, af3, af4...)
  results_df <- as.data.frame(results_list)
  names(results_df) <- paste0("af", seq_len(n_iterations))
  
  # bring back row labels from the original matrix for reference
  results_df <- cbind(label = raw_data[[1]], results_df)
  
  return(results_df)
}

