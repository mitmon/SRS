# Climate index

# This library contains the climate index parameters.

# Creation date: Feb 23, 2022
# Last updated: Feb 23, 2022

#' Basic climate rating
#'
#' The basic climate rating is a designed to return the point deduction
#' for the moisture component and temperature factors. The max deduction is
#' taken for between the moisture factor and the temperature factor.
#' @param ratingTableArray Rating table lower and upper bounds for deduction.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @return Deduction points for the basic climate rating.
#' @export
basicClimateRating <- function(ratingTableArray,ppe,temperatureFactor){

  moistureFactor <- moistureComponent(ratingTableArray[1], ppe)

  # Need to determine if it's EGDD or CHU

  temperatureFactor <- egddComponent(ratingTableArray[2], temperatureFactor)
  temperatureFactor <- chuComponent(ratingTableArray[2],temperatureFactor)

  return(min(moistureFactor,temperatureFactor))

}

#' Climate modifying factors
#'
#' The climate modifying factors is a designed to return the percentage deduction
#' for early spring moisture, excess fall moisture, and early fall frost. Max
#' deduction for each modifying factor is limited to 10% max modification.
#' @param ratingTableArray Rating table lower and upper bounds for deduction.
#' @param ppeSpring Precipitation minus potential evapotranspiration for spring
#' @param ppeFall Precipitation minus potential evapotranspiration for fall
#' @return
#' @export
climateModifyingFactors <- function(x){

  # Early spring moisture
  esm <- esmComponent(inputArray, ppeSpring)
  # Excess fall moisture
  efm <- efmComponent(inputArray, ppeFall)
  # Early fall frost
  # Not being used right now. Adding in future versions.
  # eff <- effComponent(inputArray, temperatureFall)

  return(sum(esm,efm))
  # Future versions use this
  # return(sum(esm,efm,eff))

}
