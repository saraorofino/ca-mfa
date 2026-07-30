## place holder for 2 file app global to load libraries, functions, and variables once for all users when the app starts.

# Load Libraries ----------------------------------------------------------
library(here)
library(tidyr)
library(purrr)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------

lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
ca_rr <- read.csv(here::here("data", "static", "bau_rr.csv")) # CHANGE DATA FRAME TO BE CA_RR (for future other state data)
incineration <- read.csv(here::here("data", "static", "incineration_clean.csv"))


consum <- read.csv(here::here("data","static","consum_total_byo_54_clean.csv")) # DELETE IN SHINY
consum_bau <- read_csv(here("data","static","consum_bau.csv")) # DELETE IN SHINY


# Source functions  -------------------------------------------------------


