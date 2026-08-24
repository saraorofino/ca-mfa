## place holder for 2 file app global to load libraries, functions, and variables once for all users when the app starts.

# Load Libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shinyWidgets) # extend shiny widget options
library(shinycssloaders)
library(rsconnect) # delete? 
library(httr)
library(xml2)
library(readxl)
library(readr)
library(tidyr)
library(janitor)
library(stringr)
library(useeior) # EPA EEIO model 
library(forcats)

# Get RDS Files  ----------------------------------------------------------

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


# Load data --------------------------------------------------------

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
ca_rr <- read.csv(here::here("data", "static", "ca_rr_pack.csv")) |>
  rename(bau_rr_sect = bau_rr)
ca_incineration <- read.csv(here::here("data", "static", "incineration_clean.csv")) # add national avg
emission_factors <- read.csv(here('data', 'static', 'emission_factors_clean.csv'))
bea_to_plastic <- read_csv(here("data", "raw", "plastic_sector_classification.csv"))
scaled_na_consumption <- read_csv(here::here("data", "raw", "scaled_na_consumption .csv")) 



# Source functions  -------------------------------------------------------

list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)


# Plot Global -------------------------------------------------------------

sector_labels <- c(
  pack = "Packaging",
  buil = "Building/Construction",
  tran = "Transportation",
  heal = "Healthcare",
  comm = "Commercial/Institutional",
  elec = "Electrical/Electronic",
  hous = "Household/Leisure/Sports",
  mach = "Machinery",
  text = "Textiles",
  othe = "Other",
  agri = "Agriculture")


# Pre-load CA as default to create outputs faster ---------------------------------------

ca_consum_bau_default <- readRDS(here::here("data", "static", "ca_consum_bau_default.rds"))


# Make list of state choices ----------------------------------------------

state_choices <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
  "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
  "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
  "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
  "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY"
)

 




