#' @title Recycling Output 
#' @param collect_recyc Data frame output of the plastic collected based on recycling rate target.
#' Plastic Recycling Yield of 70% widely used assumption, see references below:
#' @references Geyer, R., Jambeck, J.R., Law, K.L., 2017. Production, use, and fate of all plastics ever made. Science Advances 3, e1700782. https://doi.org/10.1126/sciadv.1700782
#' @references Plastics Europe, 2024. The Circular Economy for Plastics—A European Analysis 2024. https://plasticseurope.org/knowledge-hub/the-circular-economy-for-plastics-a-european-analysis-2024/
#' @references  Pottinger, A.S., Geyer, R., Biyani, N., Martinez, C.C., Nathan, N., Morse, M.R., Liu, C., Hu, S., de Bruyn, M., Boettiger, C., Baker, E., McCauley, D.J., 2024. Pathways to reduce global plastic waste mismanagement and greenhouse gas emissions by 2050. Science 386, 1168–1173. https://doi.org/10.1126/science.adr3837
#' @description
#' Calculates the output of secondary plastic from the state due to recycling rate changes using the recycling yield estimate of 70%. 

calc_recyc_output <- function(collect_recyc)
{
  recyc_output <- collect_recyc |>
    mutate(mt_secondary_plastic_output = mt_plastic_collect * 0.7) |>
    select(year, sector, mt_secondary_plastic_output)
  return(recyc_output)
}
  
