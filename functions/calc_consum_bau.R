

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
## This + static files should be moved all into one script
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)



bea_to_plastic <- read_xlsx(path = file.path(here::here("data/raw/plastic_sector_classification.xlsx")),
                            sheet = "plastic_sector_classification") 
scaled_na_consumption <- read_csv(here::here("data", "raw", "scaled_na_consumption .csv")) 



calc_consum_bau <- function(bea_to_plastic, state_abbr, consumption_element, scaled_na_consumption, n_iterations) {

  # 00 Sector percentages pre-processing ---------------------------------------
  props <- preprocess_sectors(bea_to_plastic=bea_to_plastic)
  
  # 01 Create State Long Data ---------------------------------------------
  download_rds_state_model(state_abbr = state_abbr) # Reactive input in shiny 

  # 02 Calculate deflated/inflated plastic intensity (m) ------------------------

  m <- calc_deflated_plastic_int(state_abbr = state_abbr) 


  # 03 Calculate State Consumption from Leontief Matrix, final demand and plastic intensity ( f * L / m) in metric tons -----------------------------

  consum_2012_2020_total <- calc_state_consum(state_abbr = state_abbr, deflated_plastic_intensity = m,
                                              consumption_element = consumption_element) # only need long format? go back to summarise 

  # 04 Forecast (do first) -------------------------------------------------------------
  # load in data; slope of change in plastic intensity from model data 2012-2020 to get change in plastic intensity 

  forecast_consum <- calc_forecast(consum_2012_2020_total, scaled_na_consumption=scaled_na_consumption)

  # 05 Hindcast consumption  ------------------------------------------------

  consum_1950_2050 <- calc_hindcast(forecast_consum)

  # 06 Calculate A matrix power series   -------------------------------------------------------

  power_series <- calc_power_series(state_abbr=state_abbr, n_iterations = n_iterations) 

  # 07 Calculate A consumption in million metric tons of plastic -------------------------------------------------------

  a_consum <-calc_a_consum(state_abbr=state_abbr, power_series, m) 


  # 08 average proportion of consumption by plastic sector ---------------------

  avg_props <- calc_plastic_sector_props(a_consum, props) 


  # 09 create consum_bau final data frame by sector --------------------------------

  consum_bau <- calc_final_bau_consum(avg_props, consum_1950_2050)

return(consum_bau)

}

## EXAMPLE: testing the function
consum_bau <- calc_consum_bau(bea_to_plastic=bea_to_plastic,
                              state_abbr='CA',
                              consumption_element = "Consumption_Complete",
                              scaled_na_consumption = scaled_na_consumption,
                              n_iterations=4)


