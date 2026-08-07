


##### Pull in all functions for sourcing and modeling bau consumption
list.files(here::here("functions"), full.names = TRUE) |>
  purrr::walk(source)

# 00 Download State USEEIO State Models v1.0 for year 2020---------------------------------------------------------

state_model <- download_state_model(state = "CA")


# 01 Pull Rho Data ---------------------------------------------------------

rho_data <- get_rho_data()

# 02 Get complete consumption ---------------------------------------------

#complete_consumption_2020 <- get_complete_consum() # F the US output needed to meet final demand directly; only 2020 available on EPA website
###### double check 2020 get function equals data frame for other states

us_consum_2012_2020 <- read_excel(
  here::here("data", "static", "complete_consumption_CAEEIO.xlsx"),
  sheet = 1   
) |> 
  dplyr::rename(year = 1) |>
  filter(stringr::str_starts(year, "Complete Consumption")) |>
  mutate(year = stringr::str_remove(year, "Complete Consumption")) |>
  mutate(year = as.numeric(year)) |> 
  pivot_longer(
    cols = -year,
    names_to = "purchasing_industry",
    values_to = "us_consumption_complete"
  )


# 03 Get A Matrix Data & Calculate 2020 power series  -------------------------------------------------------
#a_power_series_2020 <- calc_power_series("A", complete_consumption, n_iterations = 4) only useful for 2020 values from EPA website 

# 04 get Leontief values  --------------------------------------------------
leontief_326 <- get_leontief_data(tab_name = "L") # only 2020 values available on EPA website, varies by state?

ca_leontief_2012_2020 <- read_excel(here::here("data", "static", "complete_consumption_CAEEIO.xlsx"),
                                                          sheet = 1) |>
  dplyr::rename(year = 1) |>
  filter(stringr::str_starts(year, "Leontif")) |>
  mutate(year = stringr::str_remove(year, "Leontif 326 ")) |>
  mutate(year = as.numeric(year)) |>
  pivot_longer(cols = -year,
               names_to = "purchasing_industry",
               values_to = "leontif")


# 05 Calculate Plastic Intensity ---------------------------------------------
m_v1 <- calc_deflated_plastic_int_v1(rho_data) # matches values in ca_plastic_consumption.xlsx tab: plastic-consumption-io
m_v2 <- calc_deflated_plastic_int_v2(rho_data) # matches values in CAEEIO_326_2012_2020

# 06 Calculate CA total consumption in tonnes ----------------------------------------

state_consum_2012_2020_v2 <-  calc_state_consum(leontif_326 = ca_leontif_2012_2020, us_consum_2012_2020 = us_consum_2012_2020, deflated_plastic_intensity = m_v2) # for CAEEIO values 

# check values against ca_plastic_consumption 

ca_consumption_check_v2 <- read.csv(here::here("data", "static", "complete_consumption_CAEEIO.csv"),
                                                        check.names = FALSE) |> # pulls values provided by EPA privately for 2012-2020
  dplyr::rename(year = 1) |>
  filter(stringr::str_starts(year, "CA ")) |>
  mutate(year = stringr::str_remove(year, "CA Plastic Consumption")) |>
  mutate(year = as.numeric(year)) |> 
  pivot_longer(cols = -year,
               names_to = "purchasing_industry",
               values_to = "ca_consumption")


check <- left_join(ca_consumption_check_v2,
                   state_consum_2012_2020_v2,
                   by = c("year", "purchasing_industry")) |>
  select("year",
         "purchasing_industry",
         "state_consum_mt",
         "ca_consumption",
         "us_consumption_complete") |>
  mutate(diff = state_consum_mt - ca_consumption) # the same 



# Calculate OEM plastic consumption ---------------------------------------
# final demand f * a326 / deflated plastic




# plastic consumption io --------------------------------------------------
io_file <- file.path(here::here("data/raw/CAEEIO_326_output_2012_2020_v4.xlsx"))

# Hindcast ----------------------------------------------------------------
# calc_hindcast <- function (state_gdp, plastic_intensity)
# consumption_mt = state_gdp * plastic_intensity

# Forecast -----------------------------------------------------------------

# Plastic Series Share ---------------------------------------------------