#' @title TEST End of Life Plastic  
#' @param recyc_output Data frame output of the plastic collected based on recycling rate targets and adjusted for yield losses at 70%. 
#' @param incineration Data frame of incineration rates, if none provided CalRecycle values will be used as default.
#' @reference  CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25). 
#' @description
#' Calculates the plastic to landfill remaining after recycling output and incineration and includes all three fates in final output.  


# upload static df --------------------------------------------------------
lifetimes <- read.csv(here::here("data","static","lifetimes_clean.csv"))
user_inputs_sb54 <- read.csv(here::here("data","static", "user_inputs_sb54.csv"))
bau_rr <- read.csv(here::here("data", "static", "bau_rr.csv")) #copy to preprocessing
incineration <- read.csv(here::here("data", "static", "incineration_clean.csv"))
consum <- read.csv(here::here("data","static","consum_total_byo_54_clean.csv"))


# upload excel model outputs ----------------------------------------------
model <- read.csv(here::here("data", "output", "sb54validation.csv"))

# create waste gen --------------------------------------------------------
wastegen <- calc_wastegen(lifetimes = lifetimes, consum = consum)

# create collect recyc ----------------------------------------------------
collect_recyc <- calc_collect_recyc(wastegen = wastegen, bau_rr = bau_rr, implement_year_rr = 2025, target_sector_rr = "pack", target_rr = 0.65, target_year_rr = 2032) 

# create recyc output -----------------------------------------------------
recyc_output <- calc_recyc_output(collect_recyc = collect_recyc)

# test eol function -------------------------------------------------------
eol <- calc_eol(wastegen = wastegen, recyc_output = recyc_output, incineration = incineration) #waste gen still pulling incorrect total   

# crosscheck model  -------------------------------------------------------
compare_wastegen <- wastegen |> 
  filter(sector == "all_sec") |> 
  select(year, mt_plastic_wastegen) |> 
  left_join(model, by = "year") |> 
  mutate(
    diff = mt_plastic_wastegen - wastegen_all,
    match = near(mt_plastic_wastegen, wastegen_all, tol = 1e-2)
  )

compare_wastegen |> arrange(desc(abs(diff)))

compare_wastegen <- model |> # waste gen wrong check in main
  select(year, wastegen_all ) |>                 
  left_join(wastegen |>  
              select(year, sector, mt_plastic_wastegen), by = "year", "sector") |> 
              filter(sector == 'all_sec') |> 
  mutate(match = near(wastegen_all, mt_plastic_wastegen, tol = 1e-2))

compare_collect_recyc<- model |> 
  select(year, collect_recyc) |>                 
  left_join(collect_recyc |> filter(sector == 'pack') |> 
              select(year, sector, mt_plastic_collect), by = "year", "sector") |> 
  mutate(match = near(collect_recyc, mt_plastic_collect, tol = 1e-2))

compare_recyc_output <- model |> 
  select(year, recyc_output) |>               
  left_join(recyc_output|> filter(sector == 'pack') |> 
              select(year, sector, mt_secondary_plastic_output), by = "year", "sector") |> 
  mutate(match = near(recyc_output, mt_secondary_plastic_output, tol = 1e-2)) 

 
compare_incin <- model |>
  mutate(incineration = incineration / 1000) |> # convert to mt from kt
  select(year, incineration) |>
  left_join(eol |>
              select(year, sector, mt_incin), by = "year", "sector") |>
  mutate(match = near(incineration, mt_incin, tol = 1e-2))


compare_landfill <- model |>  # NOT CORRECT 
  select(year, landfill) |>               
  left_join(eol|> 
              select(year, mt_plastic_landfill), by = "year") |> 
  mutate(match = near(landfill, mt_plastic_landfill, tol = 1e-2))
