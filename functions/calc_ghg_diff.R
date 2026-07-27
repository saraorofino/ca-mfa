#' @title Calculating differences in Greenhouse Gas production
#' @param ghg_prod Dataframe output of calc_ghg: Greenhouse gasses produced by production/consumption under the custom policy scenario. 
#' @param ghg_prod_bau Dataframe output of calc_ghg: Dataframe output of calc_ghg: Greenhouse gasses produced by production/consumption under the business as usual scenario
#' @param ghg_eol Dataframe output of calc_ghg: Greenhouse gasses produced by end of life treatment (landfill, recycling, and incineration) under the custom policy scenario.
#' @param ghg_eol_bau Dataframe output of calc_ghg: Greenhouse gasses produced by end of life treatment (landfill, recycling, and incineration) under the business as usual scenario.
#' @param ghg_prim_prod Dataframe output of calc_ghg: Greenhouses gasses avoided by the displacement of primary production due to recycling output under the custom policy scenario. Output as a negative number.
#' @param ghg_prim_prod_bau Dataframe output of calc_ghg: Dataframe output of calc_ghg: Greenhouses gasses avoided by the displacement of primary production due to recycling output under the business as usual scenario. Output as a negative number.
#' @description
#' This function calculates cummulative total greenhouse gas produced under the categories production/consumption, end of life treatments, as well as the avoided greenhouse gasses due to displacement of primary production (negative values). Calculates the difference in greenhouse gas produced under these categories between the business as usual and custom policy scenarios.
#' @return A summary data frame with columns for the total greenhouse gases (in MtCO2e) produced by production, end of life (eol), and avoided primary production (a negative value). Also contains the difference in greenhouse gasses produced by these 3 categories, and the cummulative difference in greenhouses gasses produced across all 3 categories.


calc_ghg_diff <- function(ghg_prod,
                          ghg_prod_bau,
                          ghg_eol,
                          ghg_eol_bau,
                          ghg_avoid_prim_prod,
                          ghg_avoid_prim_prod_bau){
  

# part 1: ghg from production ---------------------------------------------

  ghg_prod_diff <- ghg_prod |> #feed in 'byo'
    filter(sector != 'all_sec') |>
    left_join(ghg_prod_bau |> 
                filter(sector != 'all_sec'),
              by = c('year', 'sector'),
              suffix = c("", "_bau")) |>  
    select(c(year, starts_with('mt_co2e'))) |> 
    summarize(ghg_prod_total = sum(mt_co2e_prod),
              ghg_prod_total_bau = sum(mt_co2e_prod_bau),
              ghg_prod_total_diff = ghg_prod_total_bau - ghg_prod_total) 
  

# part 2: ghg from end of life --------------------------------------------

  ghg_eol_diff <- ghg_eol |>  
    left_join(ghg_eol_bau,
              by = 'year',
              suffix = c("","_bau")) |> 
    select(c(year, starts_with('mt_co2e'))) |> 
    summarize(ghg_eol_total = sum(mt_co2e_eol),
              ghg_eol_total_bau = sum(mt_co2e_eol_bau),
              ghg_eol_total_diff = ghg_eol_total_bau - ghg_eol_total)
  

# part 3: ghg from avoided primary production -----------------------------

  ghg_prim_prod_diff <- ghg_prim_prod |> 
    filter(sector != 'all_sec') |> 
    left_join(ghg_prim_prod_bau |> 
                filter(sector != 'all_sec'),
              by = 'year',
              suffix = c("","_bau")) |> 
    select(c(year, starts_with('mt_co2e'))) |> 
    summarize(ghg_avoid_prim_prod_total = sum(mt_co2e_avoidprod),
              ghg_avoid_prim_prod_total_bau = sum(mt_co2e_avoidprod_bau),
              ghg_avoid_prim_prod_total_diff = ghg_avoid_prim_prod_total_bau - ghg_avoid_prim_prod_total
    )


# part 4: combine all 3 _diff summary dataframes --------------------------

  ghg_diff <- bind_cols(
    ghg_prod_diff,
    ghg_eol_diff,
    ghg_prim_prod_diff
  ) |> 
    mutate(total_diff =ghg_prod_total_diff + ghg_eol_total_diff + ghg_avoid_prim_prod_total_diff )
  
  return(ghg_diff)  
  
}