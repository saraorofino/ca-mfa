# Test ability to download excel files from EPA website
library(rvest)
library(xml2)
library(htmltools)
library(stringr)
library(purrr)
library(readr)

# Define paths 
main_epa_url <- "https://catalog.data.gov/dataset/useeio-state-models-v1-0-for-2020"
output_dir <- file.path(here::here("data/raw/epa-state-models"))

# Write functions
# Extract all the URLs in the main page 
extract_download_urls <- function(webpage_url) {
  webpage <- read_html(webpage_url)
  download_links <- webpage |>  
    html_nodes("a") |>   
    html_attr("href") |> 
    str_subset("\\.xlsx")
  
  return(download_links)
}

## Download the excel files 
download_files <- function(url){
  
  file_name <- gsub("^.*/", "", url)
  out <- paste0(output_dir, "/", file_name)
  
  # Download files if they don't exist
  if(!file.exists(out)){
    download.file(url=url, destfile=out, mode="wb")
  }
}

# All URLS for excel documents
model_urls <- extract_download_urls(main_epa_url)

# Download the excel file
download_xlsm <- model_urls |> 
    walk(download_files)



