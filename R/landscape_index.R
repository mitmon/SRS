# Landscape index

# This library contains the landscape index parameters.

# Creation date: Feb 23, 2022
# Last updated: Feb 24, 2022

#' Basic landscape rating
#'
#' The basic landscape rating returns the point deduction for the percent
#' slope and landscape type.
#' @param slopePercent Slope percentage.
#' @param slopeLength Slope length based on LS calculation.
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @return Deduction points for the basic climate rating.
#' @export
basicLandscapeRating <- function(slopePercent,slopeLength){
  if(is.na(slopePercent) || is.na(slopeLength)){
    pointDeduct <- 0
  }
  # Simple landscapes. Nominal slope lengths equal to or over 100m
  else if(slopeLength >= 100){
    pointDeduct <- 66.560928 + 2.156809 * slopePercent - sqrt((-38.609623 + 2.156809 * slopePercent) ^ 2 + 54.877374 ^ 2)
  }
  # Complex landscapes. Nominal slope lengths less than 100m
  else if(slopeLength < 100){
    pointDeduct <- 128.20977 + 8.5212186 * slopePercent - sqrt((24.148183 + 8.5212186 * slopePercent) ^ 2 + 126.64124 ^ 2)
  } else {
    pointDeduct <- 0
  }
  return(pointDeduct)
}

#' Interim landscape rating
#'
#' The interim landscape rating returns the point deduction as a percent deduction
#' from the basic landscape rating. This parameter is currently not being used.
#' @param surfaceStoniness Surface stoniness in annual removal (cubic m/ha)
#' @param coarseFragment Coarse fragment content as a percentage of volume.
#' @param woodContent Wood content as a percentage of volume.
#' @return Deduction points for the interim landscape rating.
#' @export
interimLandscapeRating <- function(surfaceStoniness,coarseFragment,woodContent){

  pointDeduct <- 0

  if(is.na(surfaceStoniness) && is.na(coarseFragment) && is.na(woodContent)){
    pointDeduct <- 0
  }
  # Surface stoniness deduction
  if(!is.na(surfaceStoniness)){
    pointDeduct <- pointDeduct + 50 * (surfaceStoniness) + 5
  }
  # Coarse fragment deduction
  if(!is.na(coarseFragment)){
    ifelse(coarseFragment >= 7.5,
           pointDeduct <- pointDeduct + (50 * coarseFragment + 5),
           pointDeduct <- pointDeduct + (0.96285714 * coarseFragment - 9 - 0.0057142857 * coarseFragment ^ 2))
  }
  # Wood content deduction
  if(!is.na(woodContent)){
    pointDeduct <- pointDeduct
  }
  return(pointDeduct)
}
