
#' @description
#' BEA industry classification 326 represents plastics and rubber products manufacturing as well as "Used" and "Other" goods. RoUS indicates the rest of the US. 
#' 


get_rho_data <- function() {
  readxl::read_excel(state_model, sheet = "Rho") |>
    rename(bea_industry = 1) |>
    dplyr::filter(stringr::str_starts(bea_industry, "326")) |>
    pivot_longer(cols = -bea_industry,
                 names_to = "year",
                 values_to = "rho")
}
  

ca_models <- load_state_models("CA", rds_files, base_url)

rho_ca <- get_model_element(ca_models, 2012, "mu", "Rho")
