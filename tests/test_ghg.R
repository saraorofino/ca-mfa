
# function name could be calc_ghg

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

#generating landfill, incineration, recyc to put in environmnet

bau_rr <- read_csv(here('data','static', 'bau_rr.csv'))

calc_collect_recyc(wastegen_byo, 
                   bau_rr = ,
                   2025,
                   0.65,
                   'pack',
                   2032)
calc_recyc_output()
calc_landfill()


emission_factors_disposal <- emission_factors |> 
  filter(type == 'disposal')

ghg_disposal |> 



