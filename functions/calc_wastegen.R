#' @title Waste Generated Function
#' @param lifetimes  Static dataframe with the product lifetime distribution of the 11 sectors.  
#' @param consum Data frame output of consumption.
#' @description This function calculates the plastic consumption under source reduction in the build your own scenario (BYO). Uses linear scaling to forecast source reduction rates based on the target source reduction policy input, and multiplies by the consumption under the business as usual scenario.
#' @return A data frame ‘wastegen’ with columns for year, sector, and megatones of plastic waste generated (mt_plastic_wastegen)

calc_wastegen <- function(
                          lifetimes,
                          consum) {

consum <- consum |> 
  filter( sector != 'all_sec') #removing all_sec in calculations

sectors <- unique(consum$sector)
years <- sort(unique(consum$year))

wastegen <- crossing( #creating a new datafram with all combinations of sectors and years
  sector = sectors,
  year = years
) |> 
mutate(
  mt_plastic_wastegen = map2_dbl(sector, year, function(current_sector, current_year) {
    
    # part 1: pull the lifetime probability values for each of the 11 sectors
    
    lifetime_values <- lifetimes |> 
      filter(sector == current_sector, year <= 70) |> 
      arrange(year) |> 
      pull(lifetime_probability)
    
    # part 2: pull out the consumption values for the 70 years leading up to the current year
    
    consum_values <- consum |> 
      filter(sector == current_sector,
             year < current_year, #ensuring it is the year prior, does not include current year
             year >= current_year - 70) |> 
      arrange(desc(year)) |> 
      select(starts_with("mt_plastic")) |> 
      pull()
    
    # ensures there are 70 years of consumption data by replacing missing values with 0 (as done in excel)
    consum_values <- c(consum_values, rep(0, 70 - length(consum_values)))
   
    # part 3: sums the product of the lifetime and the consumption for the current year
    sum(consum_values * lifetime_values) 
  })
)

# calculate the total across all sectors for each year

wastegen_all_sec <- wastegen |> 
  group_by(year) |> 
  summarize(mt_plastic_wastegen = sum(mt_plastic_wastegen), .groups = "drop") |> 
  mutate(sector = "all_sec")

#bind rows back to original dataframe
wastegen <- bind_rows(wastegen, wastegen_all_sec) |> 
  arrange(desc(year))

return(wastegen)

}