library(shiny)
library(httr)
library(xml2)

# ---- Config ---------------------------------------------------------------

base_url   <- "https://dmap-data-commons-ord.s3.amazonaws.com/"
list_url   <- paste0(base_url, "?list-type=2&prefix=USEEIO-State/")

# Regex matches any two-letter state acronym + any two-digit year, e.g.
# "USEEIO-State/CTEEIOv1.0-s-12.rds"
file_pattern <- "([A-Z]{2})EEIOv1\\.0-s-(\\d{2})\\.rds$"

# ---- Fetch the bucket file listing once at app startup --------------------

get_bucket_keys <- function() {
  resp <- GET(list_url)
  doc  <- read_xml(content(resp, as = "text", encoding = "UTF-8"))
  xml_text(xml_find_all(doc, "//*[local-name()='Key']"))
}

all_keys  <- get_bucket_keys()
rds_files <- all_keys[grepl(file_pattern, all_keys)]

# Pull out every distinct state acronym present in the bucket, to populate
# the dropdown dynamically (no hardcoded state list to maintain).
available_states <- sort(unique(sub(paste0(".*", file_pattern), "\\1", rds_files)))

# ---- Helper: load every year (2012-2020) for one state --------------------

load_state_models <- function(state_abbr, rds_files, base) {
  target <- rds_files[grepl(paste0(state_abbr, "EEIOv1\\.0-s-"), rds_files)]
  if (length(target) == 0) stop("No files found for state: ", state_abbr)
  
  models <- list()
  for (key in target) {
    yr_suffix <- sub(paste0(".*", file_pattern), "\\2", key)
    year <- paste0("20", yr_suffix)
    
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    
    download.file(paste0(base, key), destfile = tmp, mode = "wb", quiet = TRUE)
    models[[year]] <- readRDS(tmp)
  }
  
  # order by year ascending
  models[order(names(models))]
}


# Test on a state ---------------------------------------------------------

ca_models <- load_state_models("CA", rds_files, base_url)


# pull matrix data --------------------------------------------------------

#' Pull a nested sub-element from a loaded USEEIO state model list
#
#'
#' @param state_models Named list of models keyed by year.
#' @param year Year to pull, as number or string.
#' @param path Character vector of nested element names to walk down,
#'   e.g. c("mu", "L") for model$mu$L.
#'
#' @return The requested nested element.
#'
get_model_element <- function(state_models, year, path) {
  
  year <- as.character(year)
  
  if (!year %in% names(state_models)) {
    stop("Year '", year, "' not found. Available years: ",
         paste(names(state_models), collapse = ", "))
  }
  
  obj <- state_models[[year]]
  
  for (nm in path) {
    if (!is.list(obj) || !nm %in% names(obj)) {
      stop("Element '", nm, "' not found while walking path [",
           paste(path, collapse = " -> "), "] for year ", year,
           ". Available at this level: ", paste(names(obj), collapse = ", "))
    }
    obj <- obj[[nm]]
  }
  
  obj
}


# pull Leontief Matrix, Rho, and A Matrix by year --------------------------------------------
#' Pull a nested sub-element from a loaded USEEIO state model list
#'
#' @param state_models Named list of models keyed by year.
#' @param year Year to pull, as number or string.
#' @param path Character vector of nested element names to walk down,
#'   e.g. c("mu", "L") for model$mu$L.
#'
get_model_element <- function(state_models, year, path) {
  
  year <- as.character(year)
  
  if (!year %in% names(state_models)) {
    stop("Year '", year, "' not found. Available years: ",
         paste(names(state_models), collapse = ", "))
  }
  
  obj <- state_models[[year]]
  
  for (nm in path) {
    if (!is.list(obj) || !nm %in% names(obj)) {
      stop("Element '", nm, "' not found while walking path [",
           paste(path, collapse = " -> "), "] for year ", year,
           ". Available at this level: ", paste(names(obj), collapse = ", "))
    }
    obj <- obj[[nm]]
  }
  
  obj
}


# Pull all model values and make long df ----------------------------------

#' Pull a nested sub-element from a loaded USEEIO state model list
#'
#' @param state_models Named list of models keyed by year.
#' @param year Year to pull, as number or string.
#' @param path Character vector of nested element names to walk down,
#'
#'
get_model_element <- function(state_models, year, path) {
  
  year <- as.character(year)
  
  if (!year %in% names(state_models)) {
    stop("Year '", year, "' not found. Available years: ",
         paste(names(state_models), collapse = ", "))
  }
  
  obj <- state_models[[year]]
  
  for (nm in path) {
    if (!is.list(obj) || !nm %in% names(obj)) {
      stop("Element '", nm, "' not found while walking path [",
           paste(path, collapse = " -> "), "] for year ", year,
           ". Available at this level: ", paste(names(obj), collapse = ", "))
    }
    obj <- obj[[nm]]
  }
  
  obj
}

#' Extract multiple sub-elements across multiple years and create named objects
#'
#' Creates objects like L_2012, Rho_2012, A_2012, ... directly in the target
#' environment (default: your global environment), AND returns everything as
#' a nested list for programmatic use.
#'
#' @param state_models Named list of models keyed by year, e.g. ca_models.
#' @param years Vector of years to pull, e.g. 2012:2020.
#' @param elements Named list mapping object-name-prefix -> path within each
#'   year's model. e.g. list(L = c("mu", "L"), Rho = c("mu", "Rho"), A = c("mu", "A"))
#' @param envir Environment to assign the loose objects into (default: caller's).
#'
#' @return Invisibly, a nested list: result[[element]][[year]]
#'
extract_model_elements <- function(state_models, years,
                                   elements = list(L   = c("L"),
                                                   Rho = c("Rho"),
                                                   A   = c("A")),
                                   envir = parent.frame()) {
  
  result <- list()
  
  for (elem_name in names(elements)) {
    result[[elem_name]] <- list()
    
    for (yr in years) {
      val <- get_model_element(state_models, yr, elements[[elem_name]])
      
      obj_name <- paste0(elem_name, "_", yr)
      assign(obj_name, val, envir = envir)
      
      result[[elem_name]][[as.character(yr)]] <- val
      message("Created: ", obj_name)
    }
  }
  
  invisible(result)
}

#' Convert a named list of matrices (one per year) into one long data frame
#'
#' @param mat_list Named list of matrices, names = years, e.g. all_elements[["L"]]
#' @param element_name Label to stamp in the "element" column (e.g. "L", "Rho", "A")
#'
#' @return A long-format data frame: row, col, value, year, element
#'
matrices_to_long_df <- function(mat_list, element_name = NULL) {
  
  df_list <- lapply(names(mat_list), function(yr) {
    m <- mat_list[[yr]]
    
    # ensure row/col names exist so they carry into the long format
    if (is.null(rownames(m))) rownames(m) <- seq_len(nrow(m))
    if (is.null(colnames(m))) colnames(m) <- seq_len(ncol(m))
    
    df <- as.data.frame(as.table(as.matrix(m)), stringsAsFactors = FALSE)
    names(df) <- c("row", "col", "value")
    df$year <- yr
    if (!is.null(element_name)) df$element <- element_name
    df
  })
  
  do.call(rbind, df_list)
}

#' Combine L, Rho, A (or any set of extracted elements) across all years
#' into one long data frame
#'
#' @param all_elements Nested list returned by extract_model_elements(),
#'   i.e. all_elements[[element]][[year]]
#'
#' @return One combined long-format data frame with columns:
#'   element, year, row, col, value
#'
combine_elements_long <- function(all_elements) {
  combined <- lapply(names(all_elements), function(elem) {
    matrices_to_long_df(all_elements[[elem]], element_name = elem)
  })
  do.call(rbind, combined)
}

# ---- Usage ------------------------------------------------------------

# ca_models <- load_state_models("CA", rds_files, base_url)

# Creates L_2012...L_2020, Rho_2012...Rho_2020, A_2012...A_2020
# directly in your environment:
extract_model_elements(ca_models, 2012:2020)

# Also returns everything as a nested list, if you want it:
all_elements <- extract_model_elements(ca_models, 2012:2020)
# all_elements[["L"]][["2012"]]
# all_elements[["Rho"]][["2020"]]

# ---- Combine into one long data frame ----------------------------------

# One element (e.g. just L) across all years:
L_long <- matrices_to_long_df(all_elements[["L"]], element_name = "L")
# columns: row, col, value, year, element

# All three elements (L, Rho, A) across all years, one big data frame:
combined_long <- combine_elements_long(all_elements)
# columns: element, year, row, col, value

# e.g. filter down to just what you need:
# combined_long[combined_long$element == "L" & combined_long$year == "2012", ]

# Structure confirmed: year -> element name directly (no extra sub-folder
# nesting), i.e. ca_models[["2012"]][["L"]], ca_models[["2012"]][["Rho"]],
# ca_models[["2012"]][["A"]]. This is now the default above.
#
# If you ever need to add a differently-nested element later, override paths
# explicitly when calling, e.g.:
# extract_model_elements(
#   ca_models, 2012:2020,
#   elements = list(L = c("L"), Rho = c("Rho"), A = c("A"), Foo = c("sub", "Foo"))
# )

# Pull all model values by year ---------------------------------------------------

#' Extract multiple sub-elements across multiple years and create named objects
#'
#' Creates objects like L_2012, Rho_2012, A_2012, ... directly in the target
#' environment (default: your global environment), AND returns everything as
#' a nested list for programmatic use.
#'
#' @param state_models Named list of models keyed by year, e.g. ca_models.
#' @param years Vector of years to pull, e.g. 2012:2020.
#' @param elements Named list mapping object-name-prefix -> path within each
#'   year's model. e.g. list(L = c("mu", "L"), Rho = c("mu", "Rho"), A = c("mu", "A"))
#' @param envir Environment to assign the loose objects into (default: caller's).
#'
#' @return Invisibly, a nested list: result[[element]][[year]]
#'
extract_model_elements <- function(state_models, years,
                                   elements = list(L   = c("L"),
                                                   Rho = c("Rho"),
                                                   A   = c("A")),
                                   envir = parent.frame()) {
  
  result <- list()
  
  for (elem_name in names(elements)) {
    result[[elem_name]] <- list()
    
    for (yr in years) {
      val <- get_model_element(state_models, yr, elements[[elem_name]])
      
      obj_name <- paste0(elem_name, "_", yr)
      assign(obj_name, val, envir = envir)
      
      result[[elem_name]][[as.character(yr)]] <- val
      message("Created: ", obj_name)
    }
  }
  
  invisible(result)
}


# Test --------------------------------------------------------------------
# Returns as indivdual objects in the environment
#extract_model_elements(ca_models, 2012:2020)

# Return as nested list 
all_elements <- extract_model_elements(ca_models, 2012:2020)
# all_elements[["L"]][["2012"]]
# all_elements[["Rho"]][["2020"]]


# Old just Leontief  ------------------------------------------------------


years <- 2012:2020

for (yr in years) {
  L_matrix <- get_model_element(ca_models, yr, c( "L"))
  assign(paste0("L_", yr), L_matrix)
}

# Now L_2012, L_2013, ..., L_2020 exist as separate objects in your environment

# ---- Alternative: keep them in a named list instead of loose objects ------
# (often easier to work with than 9 separate variable names)

L_by_year <- setNames(
  lapply(years, function(yr) get_model_element(ca_models, yr, c( "L"))),
  paste0("20", substr(years, 3, 4))
)

# access like:
# L_by_year[["2012"]]
# L_by_year[["2020"]]
