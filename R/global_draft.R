## place holder for 2 file app global to load libraries, functions, and variables once for all users when the app starts.

# Load Libraries ----------------------------------------------------------
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
ca_rr <- read.csv(here::here("data", "static", "ca_rr.csv")) # CHANGE DATA FRAME TO BE CA_RR (for future other state data)
incineration <- read.csv(here::here("data", "static", "incineration_clean.csv")) # CHANGE TO BE ca_incineration
emission_factors <- read.csv(here('data', 'static', 'emission_factors_clean.csv'))

#DELETE
consum_bau <- read.csv(here::here("data","static","consum_bau.csv")) # DELETE IN SHINY

# Source functions  -------------------------------------------------------
###### OR put all functions in R/ folder for shiny 
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)

