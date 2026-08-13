
calc_final_bau_consum<- function(avg_props, consum_1950_2050) {
 
 bau_consum <- consum_1950_2050 |>
    cross_join(avg_props) |>
    mutate(sector_consum = avg_prop * total_consum_mt) |>
    select(-avg_prop) |>
    pivot_wider(
      names_from = plastic_sector,
      values_from = sector_consum
    )
  
  bau_consum <- bau_consum |>
    rename(agri = Agriculture,
           comm = `Commercial / Institutional`,
           buil =  Construction, 
           elec= `Electrical/Electronic`, 
           heal = Healthcare , 
           hous = `Household / Leisure / Sports`, 
           mach = Machinery, 
           pack = Packaging,
           text = Textiles, 
           tran = Transportation,
           othe = Other
           )
  
  }