
library(roxygen2)

#' @name energy
#' @description
#' This is a basic function to compute energy, in kWh, produced from a photovoltaic system. 
#' @param A solar panel areas in meters squared
#' @param r panel yield (0-1), AKA manufacture efficiency (Default: 0.2)
#' @param PR performance ratio (0-1) (Default: 0.75)
#' @param H annual average solar radiation (kWh)
#' @returns A numeric.

energy <- function(A, r, H, PR){
  
  if(missing(r)) r = 0.2
  
  if(missing(PR)) PR = 0.75
  
  E = A * r * H * PR
  
  return(E)
  
}