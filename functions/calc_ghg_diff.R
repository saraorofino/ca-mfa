
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
  )
  
  return(ghg_diff)  
  
  
  
  
  
  
  
  
  
}