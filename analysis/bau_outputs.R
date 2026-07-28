##### Create data frame outputs for BAU for use in policy scenarios 

library(dplyr)
library(tidyr)
library(purrr)

# Source Functions & Static Data --------------------------------------------------------
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv")) 
incineration <- read.csv(here::here("data","static","incineration_clean.csv"))
ca_rr <- read.csv(here::here("data","static","bau_rr.csv")) # make dynamic to add national averages 

emission_factors < read.csv(here::here("data","static","emission_factors.csv")) ####### clear environment and check this still works??? 

# Consumption Placeholder --------------------------------------------------

consum_bau <- read.csv(here::here("data","static","consum_bau.csv")) 

consum_bau_summary <- consum_bau |>  # add all_sec to data frame
  group_by(year) |> 
  summarize(mt_plastic_bau = sum(mt_plastic_bau), .groups = "drop") |> 
  mutate(sector = "all_sec")

consum_bau <- bind_rows(consum_bau, consum_bau_summary)


# Waste Generation --------------------------------------------------------

wastegen_bau <- calc_wastegen(lifetimes = lifetimes, consum = consum_bau) # Error message sectors not found


# End of Life -------------------------------------------------------------

#total recycling 
collect_recyc_bau <- calc_collect_recyc(wastegen = wastegen_bau, bau_rr = ca_rr, target_sector_rr = 'pack')

recyc_output_bau <- calc_recyc_output(collect_recyc = collect_recyc_bau)

# total end of life
eol_bau <- calc_eol(wastegen = wastegen_bau, recyc_output = recyc_output_bau, incineration = incineration)


# Greenhouse Gases --------------------------------------------------------
ghg_bau <- calc_ghg(consum = consum_bau, eol = eol_bau, emission_factors = emission_factors, target_sector = 'pack', implement_year = 2025) # make reactive in shiny 




