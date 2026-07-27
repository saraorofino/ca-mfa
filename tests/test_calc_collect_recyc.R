#' @title Collected Recycling Business-as-Usual
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @reference CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25).
#' @description
#' Calculates the weight of recycling collected in each sector, currently only packaging recycled,  and across all sectors per year. Uses the waste generation per year dataframe (wastegen) and the static recycling collection rate under business as usual dataframe (bau_rr).

# load data ---------------------------------------------------------------
library(tidyverse)
library(here)
library(dplyr)

bau_rr <- read_csv(here("data", "static", "bau_rr.csv")) #copy to preprocessing
wastegen_bau <- read_csv(here("data", "static", "wastegen_bau.csv"))
wastegen <- read_csv(here("data", "static", "wastegen_54.csv"))
incineration <- read_csv(here("data", "static", "incineration.csv"))

# preprocessing bau -------------------------------------------------------
# clean data
wastegen_bau <- pivot_longer(
  wastegen_bau,
  cols = -year,
  # everything except year
  names_to = "sector",
  values_to = "mt_plastic_wastegen"
)

wastegen <- pivot_longer(
  wastegen,
  cols = -year,
  # everything except year
  names_to = "sector",
  values_to = "mt_plastic_wastegen"
)

# test functions -----------------------------------------------------------
collect_recyc <- calc_collect_recyc(
  wastegen = wastegen,
  bau_rr = bau_rr ,
  implement_year_rr = 2025 ,
  target_rr = 0.65,
  target_sector_rr = 'pack',
  target_year_rr = 2032
)


# Test values against Excel model 
collect_recyc_model <- read_csv(here("data", "static", "collect_recyc_model.csv")) 
                                    
collect_recyc$mt_plastic_collect == collect_recyc_model$Total

# recyc_output test
recyc_output <- calc_recyc_output(collect_recyc)

# eol test
eol <- calc_eol(wastegen, recyc_output, incineration)


# BAU df to run avoid landfill ------------------------------------------------------

collect_recyc_bau <- calc_collect_recyc(wastegen_bau, bau_rr, target_sector_rr = 'pack')

recyc_output_bau <- calc_recyc_output(collect_recyc_bau)

landfill_bau <- calc_landfill(wastegen_bau, recyc_output_bau, incineration) # currently using same incineration for both policy & BAU scenarios


# avoided landfill test ---------------------------------------------------


avoid_landfill <- calc_avoid_landfill(landfill, landfill_bau)

