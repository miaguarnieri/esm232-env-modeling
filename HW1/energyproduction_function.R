
library(roxygen2)

#' @name pv_energy
#' @description
#' This is a basic function to compute energy, in kWh, produced from a photovoltaic system. 
#'  
#' @param A solar panel areas in meters squared
#' @param r panel yield (0-1), AKA manufacture efficiency (Default: 0.2)
#' @param PR performance ratio (0-1) (Default: 0.75)
#' @param H annual average solar radiation (kWh)
#' @returns E, a numeric value for energy, in kWh.

pv_energy <- function(A, r = 0.2, H, PR = 0.75){
  
  E = A * r * H * PR
  
  return(E)
  
}