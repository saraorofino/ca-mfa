
# function name could be calc_ghg
# need to filter all of this to after 2020***

emission_factors <- read_csv(here('data', 'static', 'emission_factors.csv'))




  
consum_sr <- consum_sr |> 
filter( sector != 'all_sec') #removing all_sec in calculations

sectors <- unique(consum$sector)
years <- sort(unique(consum$year))


# calculating ghg_prod ----------------------------------------------------

emission_factors_prod <- emission_factors |> 
  filter(type == 'both')

ghg_prod <- consum_sr |> 
  inner_join(emission_factors_prod, by = "sector") |> 
  mutate( mt_co2e_prod = mt_plastic_sr * emission_factor) |> 
  select(c('year', 'sector', 'mt_co2e_prod'))
  
ghg_prod_allsec <- ghg_prod |> 
  group_by(year) |> 
  summarize(mt_co2e_prod = sum(mt_co2e_prod), .groups = "drop") |> 
  mutate(sector = "all_sec")
  
ghg_prod <- bind_rows(ghg_prod, ghg_prod_allsec) |> 
    arrange(desc(year),sector)


# ghg_disposal ------------------------------------------------------------

#generating landfill, incineration, recyc to put in environment (remove in function)

bau_rr <- read_csv(here('data','static', 'bau_rr.csv'))
incineration <- read_csv(here('data', 'static', 'incineration.csv'))



collect_recyc <-calc_collect_recyc(wastegen_byo, 
                   bau_rr = bau_rr ,
                   2025,
                   0.65,
                   'pack',
                   2032)
recyc_output <- calc_recyc_output(collect_recyc)
landfill <-calc_landfill(wastegen_byo, recyc_output, incineration )

#-- start 

emission_factors_disposal <- emission_factors |> 
  filter(type == 'disposal')

landfill_ef <- emission_factors_disposal |> 
  filter(sector == 'landfill') |> 
  pull(emission_factor)
incineration_ef <- emission_factors_disposal |> 
  filter(sector == 'incineration') |> 
  pull(emission_factor)
recyc_ef <- emission_factors_disposal |> 
  filter(sector == 'recycling') |> 
  pull(emission_factor)

ghg_disposal <- landfill |> 
  inner_join(incineration, by = 'year') |> 
  inner_join(recyc_output, by = 'year') |>  #joining landfill, recyc_out, and incineration into a single DF
  select(!starts_with('sector')) |> 
  #calculating co2e for each disposal type per year
  mutate(mt_co2e_landfill = mt_plastic_landfill * landfill_ef ) |> 
  mutate(mt_co2e_incineration = incin_mt * incineration_ef ) |>  #should we name columns different?
  mutate(mt_co2e_recyc = mt_secondary_plastic_output * recyc_ef ) |> 
  #summing all 3 disposal types together
  mutate(mt_co2e_disposal = mt_co2e_landfill + mt_co2e_incineration + mt_co2e_recyc)


# ghg avoided production --------------------------------------------------

target_sector <- 'pack' #using packaging as placeholder
#need 

emission_factors_avoidprod <- emission_factors |> 
  filter(type == 'virgin') |> 
  filter(sector == target_sector) |> 
  pull(emission_factor) 

avoid_prod_ghg <- recyc_output |> 
  mutate(mt_co2e_avoidprod = mt_secondary_plastic_output * 0.8 * emission_factors_avoidprod)

