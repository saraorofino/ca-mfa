#' @title Plastic Scrap Input to State
#' @param rc_perc_byo data frame output of consumption after recycled content policy
#' @param recyc_yield Material loss rate from gathering recycled plastic to finished recycled content product. 
#' @param ca_scrap_consump In-state percent of scrap consumption  
#' @param summary Logical. If 'FALSE' (default), returns detailed data frame by year and sector. If 'TRUE', returns a summary data frame with total scrap across 1950 to 2050. 
#' @description
#' This code calculates scrap input to the state using the post consumer recycled content production target inputs and the recycling yield loss to work backwards to find the total amount of recycled plastic input. 
#' @return A detailed data frame 'scrap_input' with columns for year, sector, and megatonnes of scrap input if summary is FALSE and a data frame 'summarized' if summary is TRUE with 1950-2050 cumulative, in-state and out-of-state scrap input in megatonnes. 


# upload data -------------------------------------------------------------
# rc_perc_byo clean long format filler data frame for output, 0 in SB 54 
library(tidyr)
library(dplyr)

rc_perc_byo<- read_csv(here("data","static","rc_perc_byo.csv"))
user_inputs_sb54 <- read_csv(here("data","static", "user_inputs_sb54.csv"))


rc_perc_byo_clean <-pivot_longer(
  rc_perc_byo,
  cols = -year,          # everything except year
  names_to = "sector",
  values_to = "mt_plastic_rc"
)

write.csv(rc_perc_byo_clean, "data/static/rc_perc_byo_clean.csv", row.names = FALSE)


# hard code scrap input ----------------------------------------------------
ca_scrap_consump <- user_inputs_sb54 |> 
  filter(name == "ca_scrap_consump") |> 
  pull(value) |> 
  as.numeric()


scrap_input <- rc_perc_byo_clean %>%
  mutate(scrap_input = mt_plastic_rc / recyc_yield)

summarized <- scrap_input %>%
  filter(sector != "all_sec") %>% # removes all sector totals per year
  summarise(total_scrap = sum(scrap_input)) %>%
  mutate(scrap_is = (total_scrap * ca_scrap_consump),
         scrap_oos = total_scrap * (1 - ca_scrap_consump))


# test function -----------------------------------------------------------

function_test <- calc_scrap_input_function(rc_perc_byo_clean, recyc_yield, ca_scrap_consump, summary = FALSE)

identical(function_test, scrap_input)
  
