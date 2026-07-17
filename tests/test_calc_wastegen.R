
install.packages("slider")
library('slider')

lifetimes <- read_csv(here('data','static','lifetimes_df.csv'))


lifetimes_clean <- lifetimes |> 
      filter(!(year == 0.1)) |>  #taking out year = 0.1, does not appear to be used in the model
      pivot_longer(cols = !"year",
                names_to = "sector",
                values_to = "lifetime" #do we want to name this column something more specific, such a proportion?
                                ) |> 
  mutate(lifetime = as.numeric(lifetime)) # converting 'lifetime' column data into numeric


write.csv(lifetimes_clean, here('data','static','lifetimes_clean.csv'))



# hard coding for a single sector and year --------------------------------

#doing this before adding moving windows, testing to see if it produces proper calculation
#running and changing target_sector and year of interest (year_oi) to test this properly

target_sector <- 'pack'
year_oi <- 1956

consum_70yrs <- consum_bau_clean |> #getting 70 years of consumption to then place side by side to 70 years of lifetimes.
  filter(
    sector == target_sector,
    year < year_oi,
    year >= year_oi - 70
  ) |>
  arrange(desc(year))

consum_70yrs_expand <- consum_70yrs |> 
  pull(mt_plastic_bau)  #extracted only 1 column in order for length() to work
length(consum_70yrs_expand) <- 70 #ensures there are 70 years to look at
consum_70yrs_expand[is.na(consum_70yrs_expand)] <- 0

#an alternative would look like this
#consum_70yrs_expand <- c( consum_70yrs$mt_plastic_bau,
#rep(0, 70 - nrow(consum_70yrs)))


wastegen_example <- sum(consum_70yrs_expand * lifetime_70yrs$lifetime)

#all of this gives the proper number


# creating a loop (will use this one in the function) ---------------------

waste_gen_looped <- data.frame(
                      year = integer(),
                      sector = character(),
                      mt_plastic_wastegen = numeric()) 

sectors <- unique(consum_bau_clean$sector)
years <- sort(unique(consum_bau_clean$year))

for (current_sector in sectors) {
  
  # part 1: pull the lifetime values for each of the 11 sectors
  
  lifetime_values <- lifetimes_clean |> #should i rename this to lifetime_dist?
    filter(
      sector == target_sector,
      year <= 70 #only uses first 70 years as per the excel model
    ) |>
    arrange(year) |>
    pull(lifetime)
  
  # part 2: pull out the consumption values for the 70 years leading up to the current year
  
  for (current_year in years) {
    
    consum_values <-  consum_bau_clean |> 
    
      filter(
      sector == current_sector,
      year < current_year, #ensuring it is the year prior, does not include current year
      year >= current_year - 70 ) |>
      arrange(desc(year)) |>
      pull(mt_plastic_bau)
    
      # ensures there are 70 years of consumption data by replacing missing values with 0 (as done in excel)
      consum_values <- c(
        consum_values,
        rep(0, 70 - length(consum_values))
      )
      
  # part 3: sums the product of the lifetime and the consumption for the current year
  
      wastegen_current_year <- sum(
        consum_values * lifetime_values) # translated sumproduct into r, multiplies the 70 years of consumption and lifetimes prior to the current year and sums them for the wastegen value
    
      waste_gen_looped <- bind_rows(
        waste_gen_looped,
        data.frame(
          year = current_year,
          sector = current_sector,
          mt_plastic_wastegen = wastegen_current_year)) |> 
        arrange(desc(year))
      }}





lifetime_70yrs <- lifetimes_clean |> #getting 70 years of lifetimes values, could probably clean datafram earlier to only include 70 yera (70+ not used in calculations)
  filter(
    sector == target_sector,
    year <= 70
  ) |>
  arrange(year) 

wastegen <- sum(consumption * lifetime_70yrs$lifetime)




# bindrows method (doesn't work)

wastegen_example <- consum_70yrs |> #multiplying across into a new row called 'weighted consumption'
  select(consumption_year = year, mt_plastic_bau) |>
  bind_cols(
    lifetime_70yrs |>
      select(age = year, lifetime = lifetime)
  ) |>
  mutate(
    weighted_consumption = #will rename, but this column will then be summed to be the waste generated in the year_oi
      mt_plastic_bau * lifetime
  ) 


consumption <- consum_70yrs$mt_plastic_bau
length(consumption) <- 70
consumption[is.na(consumption)] <- 0

wastegen <- sum(consumption * lifetime_70yrs$lifetime)








wastegen_example <- wastegen_example |>
  summarize(
    mt_plastic_waste_bau =
      sum(weighted_consumption, na.rm = TRUE)
  )
  
  

 