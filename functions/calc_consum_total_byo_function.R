#' @title Total consumption under the Build Your Own (BYO) scenario
#' @param consum_total_byo data frame output of consumption after source reduction policy
#' @param rc_perc_byo data frame output of consumption after recycled content policy
#' @param ca_scrap_consump In-state percent of scrap consumption
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total mt_plastic_avoided across all years and the amount avoided in-state vs. out-of-state.
#' @description
#' This function calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year recycled content amount from the consumption levels
#' @return A data frame with columns for year, sector, and metric tons of avoided virgin plastic called mt_plastic_virgin for detailed. A summary data frame if summary is TRUE, with the total, in-state and out-of-state avoided production based on both recycled content and source reduction policies.


calc_consum_total_byo_function <- function(consum_bau_clean, user_inputs_sb54) { 
  

# extract user inputs -----------------------------------------------------

  target_year_sr <- user_inputs_sb54 |> 
    filter(name == "target_year_sr") |> 
    pull(value) |> 
    as.numeric()
  
  target_sr <- user_inputs_sb54 |> 
    filter(name == "target_sr") |> 
    pull(value) |> 
    as.numeric()
  
  baseline_year <-  user_inputs_sb54 |> 
    filter(name == "baseline_year") |> 
    pull(value) |> 
    as.numeric()
  
  implement_year <-  user_inputs_sb54 |> 
    filter(name == "implement_year") |> 
    pull(value) |> 
    as.numeric()
  
  target_sector <-  user_inputs_sb54 |> 
    filter(name == "target_sector") |> 
    pull(value)


# creating reduction multipliers ------------------------------------------

  consum_total_byo <- consum_bau_clean |> 
    #adding a column 'reduction factor' using linear scale based on inputs
    mutate(reduction_multiplier = ifelse(
      year <= target_year_sr,
      (1 - target_sr * 
         (year - baseline_year) / (target_year_sr - baseline_year)),
      (1-target_sr))) 

# calculating consumption by sector ---------------------------------------
    
   consum_total_byo <- consum_total_byo |> 
    mutate(
      mt_plastic_byo = case_when(
        
        sector == target_sector & year >= implement_year ~ 
          (consum_bau_clean |> 
             #multiply the BAU consumption from the implementation year by reduction multiplier
             filter(year == baseline_year, sector == target_sector) |> #change to match
             pull(mt_plastic_bau) * reduction_multiplier),
        
        TRUE ~ mt_plastic_bau #otherwise default to bau values
      )) |> 
    select(!c(mt_plastic_bau, reduction_multiplier))
  

# calculating total across all sectors ------------------------------------

  all_sec <- consum_total_byo |>  #calculating the totals per year to later bind with full dataframe
    group_by(year) |> 
    summarize(
      sector = "all_sec",
      mt_plastic_byo = sum(mt_plastic_byo),
      .groups = "drop")
  
  consum_total_byo <- bind_rows(consum_total_byo, all_sec) |> 
    arrange(desc(year), sector == "all_sec", sector) #binding totals back 
  
}



