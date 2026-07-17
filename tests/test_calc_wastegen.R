
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
  
  

 