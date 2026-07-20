#' @title Waste Generated Function
#' @param lifetimes  Static dataframe with the product lifetime distribution of the 11 sectors.  
#' @param consum Data frame output of consumption.
#' @description This function calculates the plastic consumption under source reduction in the build your own scenario (BYO). Uses linear scaling to forecast source reduction rates based on the target source reduction policy input, and multiplies by the consumption under the business as usual scenario.
#' @return A data frame ‘wastegen’ with columns for year, sector, and megatones of plastic waste generated (mt_plastic_wastegen)

calc_wastegen <- function(
                          lifetimes,
                          consum) {

# creating a dataframe to add looped values to, pulling unique values.  

wastegen <- data.frame(
  year = integer(),
  sector = character(),
  mt_plastic_wastegen = numeric())  

sectors <- unique(consum$sector)
years <- sort(unique(consum$year))

for (current_sector in sectors) {
  
  # part 1: pull the lifetime values for each of the 11 sectors
  
  lifetime_values <- lifetimes |> #should i rename this to lifetime_dist?
    filter(
      sector == current_sector,
      year <= 70 #only uses first 70 years as per the excel model
    ) |>
    arrange(year) |>
    pull(lifetime)
  
  # part 2: pull out the consumption values for the 70 years leading up to the current year
  
  for (current_year in years) {
    
    consum_values <-  consum |> 
      
      filter(
        sector == current_sector,
        year < current_year, #ensuring it is the year prior, does not include current year
        year >= current_year - 70 ) |>
      arrange(desc(year)) |>
      select(starts_with("mt_plastic")) |>
      pull() # would it be better here to have "consum_col" as an input in the function?
    
    # ensures there are 70 years of consumption data by replacing missing values with 0 (as done in excel)
    consum_values <- c(
      consum_values,
      rep(0, 70 - length(consum_values))
    )
    
    # part 3: sums the product of the lifetime and the consumption for the current year
    
    wastegen_current_year <- sum(
      consum_values * lifetime_values) # translated sumproduct into r, multiplies the 70 years of consumption and lifetimes prior to the current year and sums them for the wastegen value
    
    wastegen <- bind_rows(
      wastegen,
      data.frame(
        year = current_year,
        sector = current_sector,
        mt_plastic_wastegen = wastegen_current_year)) 
      
  }}

wastegen <-wastegen |> 
  arrange(desc(year))

return(wastegen)

}