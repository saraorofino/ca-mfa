#' @title Collected Recycling Business-as-Usual 
#' @param wastegen Data frame output of the waste generated based on consumption and disposal lifetimes.
#' @param bau_rr Recycling rate for the packaging sector per year based on CalRecycle data.
#' @reference PLACEHOLDER FOR CAL RECYCLE INFO


# load data ---------------------------------------------------------------
library(tidyverse)
library(here)
library(dplyr)


bau_rr <- read_csv(here("data","static","bau_rr.csv"))
wastegen <- read_csv(here("data","static",".csv"))


# hard code ---------------------------------------------------------------


