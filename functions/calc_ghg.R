#' @title Greenhouse Gasses
#' @param Consum: Data frame output of consumption.
#' @param emission_factors: Static dataframe with the emission factors (in MtCO2 equivalent / Mt resin) for 11 sectors, and End of Life (recycling, landfill, and incineration).
#' @param eol: Dataframe output of calc_eol.
#' @param target_sector: The target sector for avoid_prim_prod.
#' @param implement_year: The year of implementation.
#' @references Pottinger, A. Samuel, et al. “Pathways to Reduce Global Plastic Waste Mismanagement and Greenhouse Gas Emissions by 2050.” Science, vol. 0, no. 0, Nov. 2024, p. eadr3837. science.org (Atypon), https://doi.org/10.1126/science.adr3837.
#' @description Calculates the Mt of CO2 equivalent emitted from production (consumption), end of life (recycling, landfill, and incineration), and the avoided primary production. 
#' @return Returns 3 dataframes, greenhouse gasses in MtCO2 equivalent from production, end of life, and avoided primary production, (ghg_prod, ghg_eol, ghg_avoid_prim_prod respectively).



calc_ghg <- function(consum,
                     emission_factors,
                     eol,
                     target_sector,
                     implement_year
                      ){
  
  consum <- consum |> 
    filter( sector != 'all_sec') |> 
      rename(mt_plastic = starts_with("mt_plastic")) #renaming so that function works with consum regardless of sr or bau
  
  # part 1: calculating ghg_prod ----------------------------------------------------
  
  emission_factors_prod <- emission_factors |> 
    filter(type == 'both') #pulling required emission factors
  
  ghg_prod <- consum |> 
    inner_join(emission_factors_prod, by = "sector") |> #joining each sector's emission factor
    mutate( mt_co2e_prod = mt_plastic * emission_factor) |>  #multiplying mt_plastic by the emission factor
    select(c('year', 'sector', 'mt_co2e_prod'))
  
  #calculating the ghg from production for all sectors and binding it back
  ghg_prod_allsec <- ghg_prod |> 
    group_by(year) |> 
    summarize(mt_co2e_prod = sum(mt_co2e_prod), .groups = "drop") |> 
    mutate(sector = "all_sec")
  
  ghg_prod <- bind_rows(ghg_prod, ghg_prod_allsec) |> 
    arrange(desc(year),sector) |> 
    filter(year > implement_year)
  

# part 2: calculating ghg_eol -----------------------------------
  
  landfill_ef <- emission_factors |> #remove_eol
    filter(sector == 'landfill') |> 
    pull(emission_factor)
  
  incineration_ef <- emission_factors |> 
    filter(sector == 'incineration') |> 
    pull(emission_factor)
  
  recyc_ef <- emission_factors |> 
    filter(sector == 'recycling') |> 
    pull(emission_factor)
  
  ghg_eol <- eol |> 
    #calculating co2e for each disposal type per year
    mutate(mt_co2e_landfill = mt_plastic_landfill * landfill_ef ) |> 
    mutate(mt_co2e_incineration = mt_incin * incineration_ef) |>  #changed incin_mt to mt_incin
    mutate(mt_co2e_recyc = mt_secondary_plastic_output * recyc_ef ) |> 
    #summing all 3 disposal types together
    mutate(mt_co2e_eol = mt_co2e_landfill + mt_co2e_incineration + mt_co2e_recyc) |> 
    filter(year > implement_year) 
  
  # part 3: calculating ghg_avoid_prim_prod --------------------------------------------------
  
 avoid_prim_prod_ef <- emission_factors |> 
    filter(type == 'virgin') |> 
    filter(sector == target_sector) |> 
    pull(emission_factor) 
  
  ghg_avoid_prim_prod <- eol |> 
    mutate(mt_co2e_avoidprod = -mt_secondary_plastic_output * 0.8 * avoid_prim_prod_ef) |> 
    filter(year > implement_year) 

  return(list(
    ghg_prod = ghg_prod,
    ghg_eol = ghg_eol,
    ghg_avoid_prim_prod = ghg_avoid_prim_prod
  ))
}