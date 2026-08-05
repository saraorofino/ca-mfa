##### Pull in all functions for sourcing and modeling bau consumption
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)

# 00 Download State USEEIO State Models v1.0 for year 2020---------------------------------------------------------

state_model <- download_state_model(state = "CA")


# 01 Pull Rho Data ---------------------------------------------------------

rho_data <- get_rho_data()

# 02 get complete consumption ---------------------------------------------
complete_consumption <- get_complete_consum() # F the output needed to meet final demand directly

# 03 Get A Matrix Data & Calculate 2020 power series  -------------------------------------------------------
a_power_series_2020 <- calc_power_series("A", complete_consumption, n_iterations = 4)

# 04 get leontif values  --------------------------------------------------
leontif_326 <- get_leontif_data(tab_name = "L")

# 05 Calculate Plastic Intensity ---------------------------------------------
m_v1 <- calc_deflated_plastic_int_v1(rho_data) # matches values in ca_plastic_consumption.xlsx tab: plastic-consumption-io
m_v2 <- calc_deflated_plastic_int_v2(rho_data) # matches values in CAEEIO_326_2012_2020

# 06 Calculate CA total consumption in tonnes ----------------------------------------
ca_consum_2020 <-  calc_state_consum(leontif_326, complete_consumption, defalted_plastic_intensity)
# leonif * f / deflated plastic 


# Calculate OEM plastic consumption ---------------------------------------
# final demand f * a326 / deflated plastic




# plastic consumption io --------------------------------------------------
io_file <- file.path(here::here("data/raw/CAEEIO_326_output_2012_2020_v4.xlsx"))

# Hindcast ----------------------------------------------------------------
# calc_hindcast <- function (state_gdp, plastic_intensity) 
# consumption_mt = state_gdp * plastic_intensity

# Forecast -----------------------------------------------------------------

# Plastic Series Share ---------------------------------------------------