## place holder for 2 file app global to load libraries, functions, and variables once for all users when the app starts.

# Load Libraries ----------------------------------------------------------
library(shiny)
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)
library(shiny)
library(tidyverse)
library(shinyWidgets) # extend shiny widget options
library(shinycssloaders)
library(rsconnect) # delete? 

# Load data --------------------------------------------------------

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
ca_rr <- read.csv(here::here("data", "static", "ca_rr.csv")) # CHANGE DATA FRAME TO BE CA_RR (for future other state data)
incineration <- read.csv(here::here("data", "static", "incineration_clean.csv")) # CHANGE TO BE ca_incineration
emission_factors <- read.csv(here('data', 'static', 'emission_factors_clean.csv'))

# Placeholder for BAU state reactive
consum_bau <- read.csv(here::here("data","static","consum_bau.csv")) # DELETE IN SHINY


# Source functions  -------------------------------------------------------
###### OR put all functions in R/ folder for shiny 
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)

