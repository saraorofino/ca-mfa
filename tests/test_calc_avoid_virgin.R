#' @title Hard Code for Avoided virgin plastic production
#' @param consum_total Data frame output of consumption after source reduction policy
#' @param rc_perc Data frame output of secondary plastic consumption from the recycled content policy.
#' @param ca_scrap_consump In-state percent of scrap consumption 
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total mt_plastic_avoided across all years and the amount avoided in-state vs. out-of-state. 
#' @description
#' This code calculates avoided plastic production based on the consumption levels after source reduction. It subtracts the sector's per year recycled content amount from the consumption levels. It also calculates the total virgin plastic consumed by taking the consumption after source reduction and subtracting the recycled content amounts per year by sector. 
#' @return A data frame called 'detailed' with columns for year, sector, metric tons of avoided virgin plastic 'mt_avoid_virgin', and metric tons of virgin plastic after recycled content targets 'mt_plastic_virgin'. A summary data frame if summary is TRUE, with the total, in-state and out-of-state avoided production based on both recycled content and source reduction policies. 
#' 
# load data ---------------------------------------------------------------
# Delete in function version 
library(tidyverse)
library(here)

consum_bau_placeholder <- read.csv(here("data","static","consum_bau.csv")) 

consum_total_byo_54 <- read.csv(here("data","static","consum_total_byo_54.csv")) 

rc_perc_byo_54 <- read.csv(here("data", "static","rc_perc_byo_54.csv"))

rc_perc_byo <- read.csv(here("data", "static", "rc_perc_byo_clean.csv")) # SB 54 doesn't have rc requirements to test

# clean filler df --------------------------------------------------------
# consum_bau clean and long format filler data frame, add in all_sec

consum_bau_clean <- pivot_longer(
  consum_bau_placeholder,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_bau"
) 

consum_bau_all <- consum_bau_clean %>% # sum all the sectors together to get all_sec
  group_by(year) %>%
  summarise(mt_plastic_bau = sum(mt_plastic_bau)) %>%
  mutate(sector = "all_sec")

consum_bau <- bind_rows(consum_bau_clean, consum_bau_all) # combine df 

write.csv(consum_bau, "data/static/consum_bau.csv", row.names = FALSE)


# consum_byo clean long format filler data frame clean 
consum_total_byo_54_clean <-  pivot_longer( # change to 
  consum_total_byo_54,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_byo"
)

write.csv(consum_total_byo_54_clean, "data/static/consum_total_byo_54_clean.csv", row.names = FALSE)

# rc_perc_byo clean long format filler data frame for output

rc_perc_byo_54_clean <-pivot_longer(
  rc_perc_byo_54,
cols = -year,          # everything except year
names_to = "sector",
values_to = "mt_plastic_rc"
)

write.csv(rc_perc_byo_54_clean, "data/static/rc_perc_byo_54_clean.csv", row.names = FALSE)

# hardcode calc avoid virgin ----------------------------------------------

#avoid_virgin <- consum_total_byo_54_clean %>%
 # left_join(rc_perc_byo_54_clean, by = c("year", "sector")) %>%
 # mutate(mt_plastic_virgin = mt_plastic_byo - mt_plastic_rc) %>%
 # select(year, sector, mt_plastic_virgin)


# test function  ----------------------------------------------------------
avoid_virgin <- calc_avoid_virgin_function(consum_bau, consum_total_byo_54_clean, rc_perc_byo, 0.5, summary = TRUE) 




