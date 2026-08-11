#' @title Download EPA EEIO State Model
#' @Description Download USEEIO state model .rds files for a given state, extract
#' L, Rho, A, and the "*_Consumption_Complete" tab for every year found,
#' and return one combined long-format data frame.
#'
#' Assumes `base_url`, `file_pattern`, and `rds_files` already exist in the
#' calling environment (set once in global.R).
#'
#' @param state_abbr Two-letter state code, e.g. "CA".
#' @param assign_env Environment to assign the result into (default: caller's
#'                    global env). Set to NULL to skip assignment and just
#'                    return the data frame.
#' @param var_name   Name of the object to create in `assign_env`
#'                    (default: "<state>_long").
#'
#' @return Invisibly, a long-format data frame with columns:
#'   element, year, row, col, value


# Configure URL to be moved to Global.R   ----------------------------------------------------------
library(shiny)
library(httr)
library(xml2)

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

# download_rds_state_model ------------------------------------------------

download_rds_state_model <- function(state_abbr,
                                     assign_env = .GlobalEnv,
                                     var_name = NULL,
                                     timeout_sec = 300,
                                     max_retries = 3,
                                     consumption_scope = c("state", "RoUS")) {
  
  consumption_scope <- match.arg(consumption_scope)
  
  # ---- 1. Filter the pre-fetched listing to the requested state -------------
  target <- rds_files[grepl(paste0(state_abbr, "EEIOv1\\.0-s-"), rds_files)]
  if (length(target) == 0) stop("No files found for state: ", state_abbr)
  
  # ---- Bump download timeout for this call, restore on exit -----------------
  old_timeout <- getOption("timeout")
  options(timeout = timeout_sec)
  on.exit(options(timeout = old_timeout), add = TRUE)
  
  # ---- Helper: download with retries -----------------------------------------
  download_with_retry <- function(url, destfile, tries = max_retries) {
    for (i in seq_len(tries)) {
      result <- tryCatch({
        download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) {
        message("Attempt ", i, " failed for ", url, ": ", conditionMessage(e))
        FALSE
      })
      if (isTRUE(result) && file.exists(destfile) && file.size(destfile) > 0) {
        return(invisible(TRUE))
      }
    }
    stop("Failed to download after ", tries, " attempts: ", url)
  }
  
  # ---- 2. Download + load each year's model ----------------------------------
  models <- list()
  for (key in target) {
    yr_suffix <- sub(paste0(".*", file_pattern), "\\2", key)
    year <- paste0("20", yr_suffix)
    
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    
    download_with_retry(paste0(base_url, key), tmp)
    models[[year]] <- readRDS(tmp)
  }
  models <- models[order(names(models))]
  
  # ---- 3. Helper: pull one element from one year's model ---------------------
  get_element <- function(model, path) {
    obj <- model
    for (nm in path) {
      if (!is.list(obj) || !nm %in% names(obj)) return(NULL)
      obj <- obj[[nm]]
    }
    obj
  }
  
  # ---- 4. Helper: melt a matrix OR a plain named vector to long format -------
  to_long <- function(m, element_name, year) {
    if (is.null(m)) return(NULL)
    
    if (is.matrix(m) || is.data.frame(m)) {
      m <- as.matrix(m)
      if (is.null(rownames(m))) rownames(m) <- seq_len(nrow(m))
      if (is.null(colnames(m))) colnames(m) <- seq_len(ncol(m))
      df <- as.data.frame(as.table(m), stringsAsFactors = FALSE)
      names(df) <- c("row", "col", "value")
    } else {
      # plain vector (e.g. DemandVectors$vectors entries)
      nms <- names(m)
      if (is.null(nms)) nms <- seq_along(m)
      df <- data.frame(row = nms, col = element_name, value = as.numeric(m),
                       stringsAsFactors = FALSE)
    }
    
    df$year    <- year
    df$element <- element_name
    df
  }
  
  # ---- 5. Extract L, Rho, A, and Consumption_Complete for every year ---------
  fixed_elements <- list(L = c("L"), rho = c("Rho"), A = c("A"))
  all_long <- list()
  
  for (yr in names(models)) {
    model <- models[[yr]]
    
    for (elem_name in names(fixed_elements)) {
      val <- get_element(model, fixed_elements[[elem_name]])
      piece <- to_long(val, elem_name, yr)
      if (!is.null(piece)) all_long[[length(all_long) + 1]] <- piece
    }
    
    # Consumption vector lives at model$DemandVectors$vectors[[<name>]]
    # Name pattern: "<year>_US-<state>_Consumption_Domestic" or
    #               "<year>_RoUS_Consumption_Domestic"
    vecs <- get_element(model, c("DemandVectors", "vectors"))
    
    if (!is.null(vecs)) {
      scope_pattern <- if (consumption_scope == "state") {
        paste0("_US-", state_abbr, "_Consumption_Complete$")
      } else {
        "_RoUS_Consumption_Domestic$"
      }
      
      cons_name <- grep(scope_pattern, names(vecs), value = TRUE)
      
      if (length(cons_name) > 0) {
        val <- vecs[[cons_name[1]]]
        piece <- to_long(val, "Consumption_Complete", yr)
        if (!is.null(piece)) all_long[[length(all_long) + 1]] <- piece
      } else {
        warning("No matching Consumption_Complete vector found for year ", yr,
                " (scope = ", consumption_scope, ")")
      }
    } else {
      warning("No DemandVectors$vectors found for year ", yr)
    }
  }
  
  combined_long <- do.call(rbind, all_long)
  rownames(combined_long) <- NULL
  
  # ---- 6. Assign only the combined data frame into the environment -----------
  if (!is.null(assign_env)) {
    if (is.null(var_name)) var_name <- paste0(state_abbr, "_long")
    assign(var_name, combined_long, envir = assign_env)
    message("Created: ", var_name, " (", nrow(combined_long), " rows)")
  }
  
  invisible(combined_long)
}
