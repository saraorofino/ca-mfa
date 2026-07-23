#' @title TEST End of Life Plastic  
#' @param recyc_output Data frame output of the plastic collected based on recycling rate targets and adjusted for yield losses at 70%. 
#' @param incineration Data frame of incineration rates, if none provided CalRecycle values will be used as default.
#' @reference  CalRecycle, 2025. Recycling and Disposal Reporting System (RDRS) WWW.document.CalRecycle Home Page. URL https://calrecycle.ca.gov/swfacilities/rdreporting/ (accessed 5.29.25). 
#' @description
#' Calculates the plastic to landfill remaining after recycling output and incineration and includes all three fates in final output.  


# upload static df --------------------------------------------------------
lifetimes <- read_csv(here::here("data","static","lifetimes.csv"))
rc_perc <- read_csv(here::here("data","static","rc_perc_byo.csv"))
user_inputs_sb54 <- read_csv(here::here("data","static", "user_inputs_sb54.csv"))
bau_rr <- read_csv(here::here("data", "static", "bau_rr.csv")) #copy to preprocessing
incineration <- read_csv(here::here("data", "static", "incineration.csv"))
consum <- read_csv(here::here("data","static","consum_total_byo_54_clean.csv"))

# create waste gen --------------------------------------------------------
wastegen <- calc_wastegen(lifetimes = lifetimes, consum = consum)

# create collect recyc ----------------------------------------------------
collect_recyc <- calc_collect_recyc(wastegen = wastegen, bau_rr = bau_rr, implement_year_rr = 2025, target_sector_rr = "pack", target_rr = 0.65, target_year_rr = 2032) 

# create recyc output -----------------------------------------------------
recyc_output <- calc_recyc_output(collect_recyc = collect_recyc)


# test eol function -------------------------------------------------------
eol <- calc_eol(wastegen = wastegen, recyc_output = recyc_output, incineration = incineration)  


