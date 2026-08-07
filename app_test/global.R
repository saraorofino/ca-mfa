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

# ---- UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("USEEIO State Model Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State", choices = available_states),
      actionButton("load", "Load All Years (2012–2020)"),
      br(), br(),
      uiOutput("status"),
      br(),
      uiOutput("year_ui")
    ),
    mainPanel(
      verbatimTextOutput("structure"),
      tableOutput("preview")
    )
  )
)
