###################
# Purpose: Run the I/O model from 2012-2019
# using plastic-relevant sectors
###################

# Libraries
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)
library(forcats)
library(parallel)

# I/O model data
io_file <- file.path(here::here("data/raw/CAEEIO_326_output_2012_2020_v4.xlsx"))
plastic_sectors <- read_csv(file.path(here::here("data/processed/industry_to_plastic_sector.csv"))) 

# Power matrix approach 
## Get all the tab names
tabs <- excel_sheets(io_file)

## Search for ones with "power" in the name 
power_tabs <- c(tabs[str_detect(tabs, "Power")])

## Function to read in and process the data for each tab 
clean_power_series <- function(x) {
  
  # Get the year to assign to final output data 
  year <- as.numeric(gsub("Power series ", "", x))
  
  io_raw <- read_xlsx(path=io_file,
                      sheet = x) |> 
    replace_na(list("Plastic Intensity ($/metric ton)" = "NA")) |> 
    janitor::row_to_names(row_number = 5) 
  
  ## First pull the final direct consumption from plastic sector (326)
  direct_from_plastic <- io_raw |>
    dplyr::rename("matrix_variable" = names(io_raw)[1]) |> # Replace the first column Year 2020 with a new name
    dplyr::filter(matrix_variable == "Final consumption from 326") |>
    # Rename the 111CA/US-CA to plastic_consumption_mt
    dplyr::rename("ca_consumption_mt" = "111CA/US-CA") |>
    # Match to final formatting (matrix_variable, sector, sector_match, plastic_consumption_mt)
    dplyr::mutate(sector_match = "326",
                  ca_consumption_mt = as.numeric(ca_consumption_mt)) |>
    dplyr::select(matrix_variable, sector_match, ca_consumption_mt)

  ## Then pull the OEM and Tier 1-4 consumption & add in the final consumption for 326
  ## Join proportions to calculate plastic sector consumption
  io_matrix <- io_raw |>
    dplyr::rename("matrix_variable" = names(io_raw)[1]) |> # Same renaming of the first column
    dplyr::filter(matrix_variable %in% c("CA Total plastic consumption", "CA OEM plastic consumption", 
                                         "CA 1st tier plastic consumption", "CA 2nd tier plastic consumption", 
                                         "CA 3rd tier plastic consumption", "CA 4th tier plastic consumption")) |>
    pivot_longer(cols = c("111CA/US-CA":"Other/RoUS"),
                 names_to = "sector",
                 values_to = "ca_consumption_mt") |>
    # remove the CA-US or RoUS so it matches to the sector lookup
    dplyr::mutate(sector_match = str_split(sector, "/", simplify=T)[,1]) |>
    # summarize by sector match to avoid any duplications
    group_by(matrix_variable, sector_match) |>
    summarize(ca_consumption_mt = sum(as.numeric(ca_consumption_mt))) |>
    ungroup() |>
    # Add in the last direct from plastic sector
    bind_rows(direct_from_plastic) |>
    left_join(plastic_sectors |>
                dplyr::select(sector_match=bea_summary, plastic_sector, prop),
              by = "sector_match") |>
    dplyr::mutate(plastic_consumption_mt = ca_consumption_mt * prop)

  # Summarize by sector and matrix variable
  io_matrix_summary <- io_matrix |>
    dplyr::mutate(plastic_sector = ifelse(str_detect(plastic_sector, "Other"), "Other - all", plastic_sector)) |>
    group_by(matrix_variable, plastic_sector) |>
    summarize(plastic_consumption_mt = sum(plastic_consumption_mt)) |>
    ungroup() |>
    dplyr::mutate(year = year) |>
    dplyr::select(year, matrix_variable, plastic_sector, plastic_consumption_mt)


  return(io_matrix_summary)
}
compare_totals <- function(x) {

  # Get the year to assign to final output data 
  year <- as.numeric(gsub("Power series ", "", x))
  
  # Read in data   
  io_raw <- read_xlsx(path=io_file,
                      sheet = x)
  
  # Keep row 28-34
  io_sub <- io_raw[c(28:34),1:2]
  names(io_sub) <- c("matrix_variable", "plastic_consumption_mt")
  
  # Make sure names match power series output 
  io_totals <- io_sub |> 
    dplyr::mutate(matrix_variable = case_when(matrix_variable == 'LF' ~ "CA Total plastic consumption",
                                              matrix_variable == 'F326' ~ "Final consumption from 326",
                                              matrix_variable == 'A326F' ~ "CA OEM plastic consumption",
                                              matrix_variable == 'A326AF' ~ "CA 1st tier plastic consumption",
                                              matrix_variable == 'A326A2F' ~ "CA 2nd tier plastic consumption",
                                              matrix_variable == 'A326A3F' ~ "CA 3rd tier plastic consumption",
                                              matrix_variable == 'A326A4F' ~ "CA 4th tier plastic consumption"),
                  year = year) |> 
    dplyr::select(year, matrix_variable, plastic_consumption_mt)
  
  return(io_totals)
}

# Get results for each year & bind together 
power_series_results <- lapply(power_tabs, clean_power_series)
full_power_series <- bind_rows(power_series_results)

# Compare totals to the spreadsheet to make sure nothing went wrong
total_results <- lapply(power_tabs, compare_totals)
totals <- bind_rows(total_results) |> 
  dplyr::mutate(plastic_consumption_mt = as.numeric(plastic_consumption_mt)) 

check <- full_power_series |> 
  group_by(year, matrix_variable) |> 
  summarize(classified_results_mt = sum(plastic_consumption_mt)) |> 
  left_join(totals) |> 
  dplyr::mutate(totals_match = ifelse(near(classified_results_mt,plastic_consumption_mt), "yes", "no")) # All yes

# Save
write_csv(full_power_series, file.path(here::here("data/output/CA_EEIO_2012_2020_power_series_by_plastic_sectors.csv")))




