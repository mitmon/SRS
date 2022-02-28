# Soil index

# This library contains the soil index parameters.

# Creation date: Feb 23, 2022
# Last updated: Feb 28, 2022

#' Soil Moisture deduction
#'
#' The moisture deduction returns the point deduction for the available water
#' holding capacity "AWHC", the surface texture, subsurface texture, and water
#' table depth. The deduction is divided into two part, the first being the
#' texture deduction using the AWHC, surface and subsurface textures. The second
#' part takes a removes a percentage of the deduction based on the water table
#' depth.
#' @param ratingTableArray Rating table lower and upper bounds for deduction.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @return Deduction points for the basic climate rating.
#' @export
# soilMoistureDeduction <- function(ratingTableArray,ppe,siltPercent,clayPercent){
#   texture <- soilTexture(siltPercent,clayPercent)
#
# }
