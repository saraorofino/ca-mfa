
# Load RDS File with 2012-2020 all state models

library(httr)
library(xml2)

resp <- GET("https://dmap-data-commons-ord.s3.amazonaws.com/?list-type=2&prefix=USEEIO-State/")
xml <- content(resp, as = "text", encoding = "UTF-8")
doc <- read_xml(xml)

# extract all the object keys (i.e. file paths within the bucket)
keys <- xml_text(xml_find_all(doc, "//d1:Key", xml_ns(doc)))
keys

# Create list of all RDS files 

rds_files <- keys[grepl("\\.rds$", keys, ignore.case = TRUE)]
rds_files

# Filter for 2012-2020 RDS only files 
target <- rds_files[grepl("EEIOv1.0-s-", rds_files)]
target

# URL download 2012-2020 based on state input by NAME

base <- "https://dmap-data-commons-ord.s3.amazonaws.com/"

ca_models <- list()

for (key in target) {
  # pull the two-digit year out of the filename, e.g. "12" from "CTEEIOv1.0-s-12.rds"
  year_suffix <- sub(".*-(\\d{2})\\.rds$", "\\1", key)
  year <- paste0("20", year_suffix)   # convert "12" -> "2012"
  
  file_url <- paste0(base, key)
  tmp <- tempfile(fileext = ".rds")
  download.file(file_url, destfile = tmp, mode = "wb")
  
  ca_models[[year]] <- readRDS(tmp)
  unlink(tmp)  # clean up temp file after loading into memory
  
  message("Loaded: ", year)
}



# scrape EPA website & extract .xlsx link for one state 2020 only ---------------------------------

download_state_model <- function(
    state,
    main_epa_url = "catalog.data.gov/dataset/useeio-state-models-v1-0-for-2020",
    output_dir = here::here("data/raw/epa-state-models")
) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # NEW: translate full state name to abbreviation if needed
  state_abb <- if (state %in% state.name) {
    state.abb[match(state, state.name)]
  } else {
    toupper(state)
  }
  
  # NEW: build expected output path up front so we can skip the download entirely if it exists
  file_name <- paste0("StateEEIOv1.0-s-20-", state_abb, ".xlsx")
  out <- file.path(output_dir, file_name)
  
  # NEW: only scrape + download if the file doesn't already exist locally
  if (!file.exists(out)) {
    webpage <- rvest::read_html(main_epa_url)
    download_links <- webpage |>  
      rvest::html_nodes("a") |>   
      rvest::html_attr("href") |> 
      stringr::str_subset("\\.xlsx")
    
    # CHANGED: filter to just the one link matching this state's abbreviation
    match_link <- download_links[stringr::str_detect(download_links, paste0("-", state_abb, "\\.xlsx$"))]
    
    if (length(match_link) == 0) {
      stop(paste("No download link found for state:", state))
    }
    if (length(match_link) > 1) {
      warning(paste("Multiple links matched for state:", state, "- using first match"))
      match_link <- match_link[1]
    }
    
    download.file(url = match_link, destfile = out, mode = "wb")
  }
  
  invisible(out)  # CHANGED: return the file path instead of the whole directory
}