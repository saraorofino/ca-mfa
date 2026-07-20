#' @title Avoided Plastic Production 
#' @param consum_bau Data frame output of consumption under the business as usual (BAU) scenario.
#' @param consum_byo Data frame output of consumption after source reduction policy.
#' @param summary Logical. If 'FALSE' (default), returns a detailed data frame. If 'TRUE', returns a summary data frame with cumulative scrap from 1950 to 2050 across all sectors.
#' @description
#' This code calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year recycled content amount from the total business as usual consumption levels.
#' @return If summary 'FALSE' a data frame 'avoid_prod' with columns for year, sector and avoided plastic production in megatons. 
#' @return If summary 'TRUE' a value 'avoid_prod_total' of 1950-2050 cumulative megatons of avoided plastic production across all sectors.  


# load data ---------------------------------------------------------------
library(tidyr)
library(dplyr)
library(here)

consum_bau_placeholder <- read.csv(here("data","static","consum_bau.csv")) 
consum_total_byo_54 <- read.csv(here("data","static","consum_total_byo_54_clean.csv")) 


# hard code ---------------------------------------------------------------

avoid_prod <- consum_total_byo_54 %>%
  left_join(consum_bau_placeholder, by = c("year", "sector")) %>%
  mutate(mt_avoid_prod = mt_plastic_bau - mt_plastic_byo) %>%
  select(year, sector, mt_avoid_prod)

avoid_prod_total <-  avoid_prod %>%
  filter(sector != "all_sec") %>% # removes all sector totals per year
  summarise(total = sum(mt_avoid_prod))


# test function -----------------------------------------------------------


avoid_prod <- calc_avoid_prod_function(consum_bau_placeholder,consum_total_byo_54, summary = FALSE) 