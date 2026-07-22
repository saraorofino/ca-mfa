
# function name could be calc_ghg

emission_factors <- read_csv(here('data', 'static', 'emission_factors.csv'))

emission_factors <- emission_factors |> 
  filter(type == 'both')


  
consum_sr <- consum_sr |> 
filter( sector != 'all_sec') #removing all_sec in calculations

sectors <- unique(consum$sector)
years <- sort(unique(consum$year))


# calculating ghg_prod ----------------------------------------------------



ghg_prod <- consum_sr |> 
  inner_join(emission_factors, by = "sector") |> 
  mutate( mt_co2e_prod = mt_plastic_sr * emission_factor) |> 
  select(c('year', 'sector', 'mt_co2e_prod'))
  
ghg_prod_allsec <- ghg_prod |> 
  group_by(year) |> 
  summarize(mt_co2e_prod = sum(mt_co2e_prod), .groups = "drop") |> 
  mutate(sector = "all_sec")
  
ghg_prod <- bind_rows(ghg_prod, ghg_prod_allsec) |> 
    arrange(desc(year),sector)


# ghg_disposal ------------------------------------------------------------





