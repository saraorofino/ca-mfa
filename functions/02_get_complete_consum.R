

get_complete_consum <- function() {
  sheets <- readxl::excel_sheets(state_model)
  tab_name <- sheets[stringr::str_starts(sheets, "2020_US") & stringr::str_ends(sheets, "_Consumption_Complete")]
  
  readxl::read_excel(state_model, sheet = tab_name) |>
    rename(purchasing_industry = 1, us_consumption_complete = 2) |>
    mutate(year = 2020)
}
