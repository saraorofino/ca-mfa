
# change 'total' to all_sec


library(tidyverse)
library(here)

consum_total_byo <- read_csv(here("data","consum_total_byo.csv"))
user_inputs_sb54 <- read_csv(here("data", "user-inputs-sb54.csv"))


# pulling values from user inputs -----------------------------------------

target_rc <- 0.4 #change back to inputs later
  
  user_inputs_sb54 |> 
  filter(name == "target_rc") |> 
  pull(value)  |> 
  as.numeric()

target_year_rc <- user_inputs_sb54 |> 
  filter(name == "target_year_rc") |> 
  pull(value)  |> 
  as.numeric()

baseline_year <- user_inputs_sb54 |> 
  filter(name == "baseline_year") |> 
  pull(value) |> 
  as.numeric()

implement_year <- user_inputs_sb54 |> 
  filter(name == "implement_year") |> 
  pull(value) |> 
  as.numeric()

target_sector <- user_inputs_sb54 |> 
  filter(name == "target_sector") |> 
  pull(value)

baseline_rc <- 0 #should we add this to inputs

year1_rc <- baseline_year+1


# calculating recycled content rate : move hard code into r folder

rc_perc_byo <- consum_total_byo |> 
  filter(!c(sector == "total")) |>  #should we have another dataframe with just totals across all sectors?
  mutate( rc_rate = 
          case_when(
            year >= implement_year & sector == 'pack' & year <= target_year_rc ~
                     baseline_rc + (target_rc - baseline_rc) * (year - year1_rc) / (target_year_rc - year1_rc), 
          year >= implement_year & sector == 'pack' & year >= target_year_rc ~ target_rc,  
          TRUE ~ 0))

rc_perc_byo <- rc_perc_byo |> 
  mutate(mt_plastic_byo_rc = mt_plastic_byo * rc_rate) |> 
  select(!c(mt_plastic_byo, rc_rate))

write.csv(rc_perc_byo, "data/static/rc_perc_byo.csv", row.names = FALSE)


# testing function --------------------------------------------------------

rc_perc_byo <- calc_rc_perc_byo_function(consum_total_byo, target_rc, target_year_rc, baseline_year, implement_year, target_sector, baseline_rc)

calc_rc_perc(consum_total_byo, 0.4, 2032, 2024, 'pack', 0)


