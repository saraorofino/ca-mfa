

get_matrix_data <- function() {
  readxl::read_excel(state_model, sheet = "L") |>
    rename(bea_industry = 1) |>
    dplyr::filter(stringr::str_starts(bea_industry, "326")) |>
    pivot_longer(cols = -bea_industry,
                 names_to = "purchasing_industry",
                 values_to = "leontief") |>
    mutate(year = "2020")
}