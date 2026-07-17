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
target_sector_rr <- 'pack'

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
      year <= baseline_year ~ bau_rr, 
      year > baseline_year & year <= target_year_rr ~
        baseline_rate + (target_rr - baseline_rate) * (year - baseline_year) / (target_year_rr - baseline_year),
      year > target_year_rr ~ target_rr,
      TRUE ~ bau_rr
    )
  )  |>

# calculating per year collection -----------------------------------------------

mutate(
  mt_plastic_collec = case_when(
    target_sector_rr == 'pack' & year > implement_year_rr ~ mt_plastic_wastegen * rr_multiplier, # is waste gen more than just RR? does it include SR and RC? Remove target_sector if only applies to packaging or leave for future?
    TRUE ~ mt_plastic_wastegen * bau_rr
  )
) 



# test function -----------------------------------------------------------

#sector == 'target_sector' can only be packaging
#calc_collect_recyc(wastegen) only have wastegen bau will be done in prepreprocessing


  



