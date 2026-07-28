##### Pull in all functions for sourcing and modeling bau consumption 

# Download State USEEIO State Models v1.0 for year 2020---------------------------------------------------------

state_model <- download_state_model(state = "CA")


# Pull Rho Data ---------------------------------------------------------

rho_data <- get_rho_data()


# Get Matrix Data -------------------------------------------------------
matrix_data_L <- get_matrix_data(tab_name = "L")

matrix_data_A <- get_matrix_data(tab_name = "A")

complete_consumption <- get_complete_consum() 


# Calculate Plastic Intensity ---------------------------------------------



# plastic consumption io --------------------------------------------------
io_file <- file.path(here::here("data/raw/CAEEIO_326_output_2012_2020_v4.xlsx"))

# Hindcast ----------------------------------------------------------------
# calc_hindcast <- function (state_gdp, plastic_intensity) 
# consumption_mt = state_gdp * plastic_intensity


# Forecast -----------------------------------------------------------------

# Plastic Series Share ---------------------------------------------------