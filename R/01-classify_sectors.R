###################
# Purpose: classify I/O model sectors into
# plastic relevant categories 
###################

# Libraries
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(janitor)
library(stringr)
library(useeior) # EPA EEIO model 
library(ggplot2)
library(forcats)

# Build the federal EPA EEIO model 
fed_io <- useeior::buildIOModel("USEEIOv2.0.1-411")

## Pull out the consumption data by sector 
fed_consumption <- as.data.frame(fed_io[["Commodities"]]$Code)
fed_consumption$us_consumption <- fed_io$q
names(fed_consumption) <- c("bea_detail", "us_consumption")

####### Roland's updated proportions Feb. 2, 2025 #######
updated_classification <- read_xlsx(path = file.path(here::here("data/raw/plastic_sector_classification.xlsx")),
                                    sheet = "plastic_sector_classification")

# Save a version of federal consumption with the category names for Roland
fed_save <- fed_consumption |> 
  left_join(updated_classification |> 
              dplyr::select(bea_sector, bea_detail, bea_description),
            by = "bea_detail") |> 
  dplyr::mutate(bea_sector = ifelse(bea_detail %in% c('562111', '562HAZ', '562212', '562213', '562910', '562920', '562OTH'),
                                    '562', bea_sector)) |> 
  filter(!is.na(bea_sector))

# Tidy up the classifications
classification_long <- updated_classification |> 
  dplyr::select(bea_summary=bea_sector, bea_detail, main_plastic_sector:secondary_sector_percent) |> 
  pivot_longer(cols = c("main_plastic_sector","secondary_plastic_sector"),
               names_to = "sector_level",
               values_to = "plastic_sector") |> 
  pivot_longer(cols = c("main_sector_percent","secondary_sector_percent"),
               names_to = "percent_level",
               values_to = "percent") |> 
  dplyr::mutate(sector_level = ifelse(str_detect(sector_level, "main"), "main", "secondary"),
                percent_level = ifelse(str_detect(percent_level, "main"), "main", "secondary")) |> 
  filter(sector_level == percent_level) |>
  dplyr::mutate(prop = percent / 100) |> 
  dplyr::select(-c(sector_level, percent_level, percent)) 

# Get new proportions for each sector 
props <- fed_consumption |> 
  left_join(classification_long |> 
              dplyr::select(bea_summary, bea_detail) |> 
              distinct(),
            by = "bea_detail") |> 
  ## Some missing -- fix 
  dplyr::mutate(bea_summary = case_when(bea_detail %in% c('562111', '562HAZ', '562212', '562213', '562910', '562920', '562OTH') ~ '562', 
                                        bea_detail == '33391A' ~ '333',
                                        bea_detail %in% c('335221', '335222', '335224', '335228') ~ '335',
                                        bea_detail %in% c('S00300', 'S00900') ~ "Other",
                                        bea_detail %in% c('S00401', 'S00402') ~ "Used",
                                        TRUE ~ bea_summary)) |>
  left_join(classification_long, by = c("bea_summary", "bea_detail")) |> 
  # Fix Sector 562 Waste management -> other 100%; Used & Other -> other 100%
  dplyr::mutate(plastic_sector = ifelse(bea_summary %in% c("562", "Used", "Other"), "Other", plastic_sector),
                prop = ifelse(bea_summary %in% c("562", "Used", "Other"), 1, prop)) |> 
  dplyr::filter(prop > 0) |> 
  # Calculate consumption in each plastic category 
  dplyr::mutate(plastic_consumption = us_consumption * prop) |> 
  group_by(bea_summary) |> 
  mutate(bea_sector_total = sum(plastic_consumption)) |> 
  ungroup() |> 
  group_by(bea_summary, bea_sector_total, plastic_sector) |> 
  summarize(plastic_consumption = sum(plastic_consumption)) |> 
  ungroup() |> 
  mutate(prop = plastic_consumption / bea_sector_total) 

write_csv(props, file.path(here::here("data/processed/industry_to_plastic_sector.csv")))
