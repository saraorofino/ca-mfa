#' @title Calculate Plastic Intensity
#' @description
#' Plastic intensity (M) is the total mass output of US plastic tons divided by the economic value of output $ 
#' rho ROUS based on 2017 value (Ingwerson et al. 2022, American Chemistry Council 2025) 1752 mass to value ratio 
#' V1 indicates the formula used in the ca_plastic_consumption sheet with 1752 as the 2017 value

calc_deflated_plastic_int <- function(state_abbr) {
  m_2017 <- 1752 # citation needed
  
  # ---- pull state long data frame dynamically ------------------------------
  df_name <- paste0(state_abbr, "_long")
  if (!exists(df_name)) {
    stop("Object '", df_name, "' not found. Run download_rds_state_model('",
         state_abbr, "') first.")
  }
  state_long <- get(df_name)
  
  # ---- pull rho values for row 326/US-<state>, year 2020 only ---------------
  rho_326 <- state_long |>
    filter(element == "rho",
           row == paste0("326/US-", state_abbr),
           year == "2020")
  
  # ---- helper: look up rho for a given "year" from the col field ------------
  get_rho <- function(yr) {
    val <- rho_326$value[rho_326$col == as.character(yr)]
    if (length(val) == 0) {
      stop("No Rho value found for col ", yr, " in ", df_name)
    }
    val[1]
  }
  
  # ---- calculate deflated/inflated plastic intensity ------------------------
  # Step 1: Calculate the anchor value, m_2020
  m_2020 <- m_2017 * get_rho(2017)
  
  # Step 2: Calculate m for every year 2012-2016 and 2018-2019 using m_2020 / rho_year
  other_years <- c(2012:2016, 2018:2019)
  
  m_df <- tibble(year = other_years) |>
    rowwise() |>
    mutate(
      rho = get_rho(year),
      m   = m_2020 / rho
    ) |>
    ungroup()
  
  # Step 3: Add back the known/anchor years for a complete 2012-2020 series
  m_df <- bind_rows(
    m_df,
    tibble(year = 2020, rho = get_rho(2020), m = m_2020),
    tibble(year = 2017, rho = get_rho(2017), m = m_2017)
  ) |>
    arrange(year)
  
  return(m_df)
}

# DELETE BELOW ------------------------------------------------------------


calc_deflated_plastic_int<- function("state_abbr") {
  m_2017 <- 1752 # citation needed 
  

# pull state long data frame ----------------------------------------------
  rho_326 <- CA_long |>
    filter(element == "rho",  row == "326/US-CA", year == "2020") 
  

#  calculate deflated/inflated plastic intensity -----------------------

  # Step 1: Calculate the anchor value, m_2020
  m_2020 <- m_2017 * get_rho(2017)
  
  # Step 2: Calculate m for every year 2012-2016 and 2018-2019 using m_2020 / rho_year
  other_years <- c(2012:2016, 2018:2019)
  
  m_df <- tibble(year = other_years) |> 
    rowwise() |> 
    mutate(
      rho = get_rho(year),
      m   = m_2020 / rho
    ) |> 
    ungroup()
  
  # Step 3: Add back the known/anchor years for a complete 2012-2020 series
  m_df <- bind_rows(
    m_df,
    tibble(year = 2020, rho = get_rho(2020), m = m_2020),
    tibble(year = 2017, rho = get_rho(2017), m = m_2017)
  ) |> 
    arrange(year)
  
  return(m_df)
}
  
  
  
  
  
  
  
  
  
  # Helper to pull a single rho value for a given year
  get_rho <- function(yr) {
    val <- rho_data$rho[rho_data$year == yr & rho_data$bea_industry != "326/RoUS"]
    if (length(val) == 0) stop("No rho value found for year ", yr)
    return(val)
  }
  
  # Step 1: Calculate the anchor value, m_2020
  m_2020 <- m_2017 * get_rho(2017)
  
  # Step 2: Calculate m for every year 2012-2016 and 2018-2019 using m_2020 / rho_year
  other_years <- c(2012:2016, 2018:2019)
  
  m_df <- tibble(year = other_years) |> 
    rowwise() |> 
    mutate(
      rho = get_rho(year),
      m   = m_2020 / rho
    ) |> 
    ungroup()
  
  # Step 3: Add back the known/anchor years for a complete 2012-2020 series
  m_df <- bind_rows(
    m_df,
    tibble(year = 2020, rho = get_rho(2020), m = m_2020),
    tibble(year = 2017, rho = get_rho(2017), m = m_2017)
  ) |> 
    arrange(year)
  
  return(m_df)
}