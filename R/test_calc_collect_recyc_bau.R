#' @title Collected Recycling Business-as-Usual 
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @reference PLACEHOLDER FOR CAL RECYCLE INFO


# load data ---------------------------------------------------------------
library(tidyverse)
library(here)
library(dplyr)

bau_rr <- read_csv(here("data","static","bau_rr.csv")) #copy to preprocessing 
wastegen_bau <- read_csv(here("data","static","wastegen_bau.csv"))
wastegen <- read_csv(here("data","static","wastegen_54.csv"))

# preprocessing bau -------------------------------------------------------
# clean data 
wastegen_bau <- pivot_longer(
  wastegen_bau,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_wastegen"
) 

wastegen <- pivot_longer(
  wastegen,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_wastegen"
) 

# hard code draft ---------------------------------------------------------------
implement_year_rr <- 2025
target_rr <- 0.65
target_year_rr <- 2032

baseline_rate <- bau_rr |> 
  filter(year == (implement_year_rr -1)) |> 
  pull(bau_rr) 

wastegen <- wastegen |> 
  filter(sector== 'pack')|>
  left_join(bau_rr, by = "year") 
  

calc_collect_recyc <- wastegen |>
  mutate(
    baseline_year = implement_year_rr - 1,
    rr_multiplier = case_when(
      target_rr <= baseline_rate ~ bau_rr,
      year <= baseline_year ~ baseline_rate,
      year > baseline_year & year <= target_year_rr ~
        baseline_rate + (target_rr - baseline_rate) * (year - baseline_year) / (target_year_rr - baseline_year),
      year > target_year_rr ~ target_rr,
      TRUE ~ bau_rr
    )
  ) # add for year < implement year 

# calculating per year collection -----------------------------------------------

mutate(
  mt_plastic_collec = case_when(
    sector == packaging & year > implement_year ~ baseline_rate * rr_multiplier,
    TRUE ~ wastegen * rr_bau
  )
) |> 
  select( -rr_multiplier)



# test function -----------------------------------------------------------

#sector == 'target_sector' can only be packaging
#calc_collect_recyc(wastegen) only have wastegen bau will be done in prepreprocessing


  



