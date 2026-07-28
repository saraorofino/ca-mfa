

# scrape EPA website & extract .xlsx link for one state ---------------------------------

download_state_model <- function(
    state,
    main_epa_url = "https://catalog.data.gov/dataset/useeio-state-models-v1-0-for-2020",
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