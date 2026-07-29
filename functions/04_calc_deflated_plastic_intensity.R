#' @description
#' Plastic intensity is the total mass output of US plastic tons divided by the economic value of output $ 
#' 

calc_deflated_plastic_intensity <- function(rho, complete_consumption)

consum_2020_dollars <-sum(complete_consumption$us_consumption_complete)
                                            
                                