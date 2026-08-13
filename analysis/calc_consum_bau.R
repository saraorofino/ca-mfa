

##### Pull in all functions for sourcing and modeling bau consumption

# Copy to Global.R --------------------------------------------------------

library(shiny)
library(httr)
library(xml2)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(useeior) # EPA EEIO model 
library(ggplot2)
library(forcats)

base_url   <- "https://dmap-data-commons-ord.s3.amazonaws.com/"
list_url   <- paste0(base_url, "?list-type=2&prefix=USEEIO-State/")

# Regex matches any two-letter state acronym + any two-digit year, e.g.
# "USEEIO-State/CTEEIOv1.0-s-12.rds"
file_pattern <- "([A-Z]{2})EEIOv1\\.0-s-(\\d{2})\\.rds$"

get_bucket_keys <- function() {
  all_keys <- character()
  token <- NULL
  
  repeat {
    url <- if (is.null(token)) {
      list_url
    } else {
      paste0(list_url, "&continuation-token=", URLencode(token, reserved = TRUE))
    }
    
    resp <- GET(url)
    doc  <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
    
    keys <- xml_text(xml_find_all(doc, "//*[local-name()='Key']"))
    all_keys <- c(all_keys, keys)
    
    is_truncated <- xml_text(xml_find_all(doc, "//*[local-name()='IsTruncated']"))
    
    if (length(is_truncated) == 0 || is_truncated != "true") break
    
    token <- xml_text(xml_find_all(doc, "//*[local-name()='NextContinuationToken']"))
    if (length(token) == 0 || token == "") break
  }
  
  all_keys
}

all_keys  <- get_bucket_keys()
rds_files <- all_keys[grepl(file_pattern, all_keys)]

# Pull out every distinct state acronym present in the bucket, to populate
# the dropdown dynamically (no hardcoded state list to maintain).
available_states <- sort(unique(sub(paste0(".*", file_pattern), "\\1", rds_files)))


# Pull all functions into environment -------------------------------------

list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)


# 00 Sector percentages pre-processing ---------------------------------------

props <- preprocess_sectors()

# 01 Create State Long Data ---------------------------------------------

download_rds_state_model(state_abbr = "CA") # Reactive input in shiny 

# 02 Calculate deflated/inflated plastic intensity (m) ------------------------

m <- calc_deflated_plastic_int("CA") 


# 03 Calculate State Consumption from Leontief Matrix, final demand and plastic intensity ( f * L / m) -----------------------------

consum_2012_2020 <- calc_state_consum(state_abbr = "CA", deflated_plastic_intensity = m,
                              consumption_element = "Consumption_Complete") # only need long format? go back to summarise 

consum_2012_2020_total <- consum_2012_2020 |>
  group_by(year) |> 
  summarise(total_consum_mt = sum(state_consum_mt)) 



# 04 Forecast (do first) -------------------------------------------------------------
# load in data; slope of change in plastic intensity from model data 2012-2020 to get change in plastic intensity 

forecast_consum <- calc_forecast(consum_2012_2020_total)

# 05 Hindcast consumption  ------------------------------------------------
# calc_hindcast <- function (state_gdp, plastic_intensity)
# consumption_mt = state_gdp * plastic_intensity
#Forecast: only population & plastic intensity tons per dollar 
#Hindcast: US consumption data scaled between GDP and population & using 2012-2020 values to check IO estimate the fixed difference between the two 
#If only interested in packaging dont even need a hindcast because its used for WASTE GENERATION estimates today 
#Pottinger et al 2024 has the US hindcast complete consumption 

consum_1950_2050 <- calc_hindcast(forecast_consum)

# 06 Calculate A matrix power series   -------------------------------------------------------

power_series <- calc_power_series(state_model = CA_long, n_iterations = 4) # change back to dynanmic state abbr

# 07 Calculate A consumption in million metric tons of plastic 

a_consum <-calc_a_consum(state_model= CA_long, power_series, m) # change back to dynanmic state abbr

a_consum_total <- a_consum |>
  group_by(year) |> 
  summarise(total_mt = sum(total_mt),
            total_tier_1 = sum(tier_1_mt),
            total_tier_2 = sum(tier_2_mt),
            total_tier_3 = sum(tier_3_mt),
            total_tier_4 = sum(tier_4_mt)) 


### Sectors by A matrix consumption for 2012 to 2020

a_sector_consum <- calc_a_sector_consum(a_consum, props)

# average out across time to apply to consum_1950_2050

# bau_consum <- avg_props * consum_1950_250

# check values against ca_plastic_consumption 

ca_consumption_check_v2 <- read.csv(here::here("data", "static", "complete_consumption_CAEEIO.csv"),
                                                        check.names = FALSE) |> # pulls values provided by EPA privately for 2012-2020
  dplyr::rename(year = 1) |>
  filter(stringr::str_starts(year, "CA ")) |>
  mutate(year = stringr::str_remove(year, "CA Plastic Consumption")) |>
  mutate(year = as.numeric(year)) |> 
  pivot_longer(cols = -year,
               names_to = "purchasing_industry",
               values_to = "ca_consumption")


check <- left_join(ca_consumption_check_v2,
                   state_consum_2012_2020_v2,
                   by = c("year", "purchasing_industry")) |>
  select("year",
         "purchasing_industry",
         "state_consum_mt",
         "ca_consumption",
         "us_consumption_complete") |>
  mutate(diff = state_consum_mt - ca_consumption) # the same 


