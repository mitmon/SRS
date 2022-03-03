# Climate index

# This library contains the climate index parameters.

# Creation date: Feb 23, 2022
# Last updated: Mar 02, 2022

#' Climate Index Main
#'
#' The climate index main calls all required function and produces the rating
#' for climate over the study site.
#' @param ratingTableArrayMC Rating table lower and upper bounds for deduction for the moisture component.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat
#' units for the study site.
#' @param ppeSpring Precipitation minus potential evapotranspiration for spring/April
#' @param ppeFall Precipitation minus potential evapotranspiration for fall/October
#' @param type If the crop uses effective growing degree days (EGDD) use EGDD else
#' if the crop uses crop heat units (CHU) use CHU.
#' @return Deduction points for the basic climate rating.
#' @export
climateIndexMain <- function(ratingTableArrayMC,ratingTableArrayESM,ratingTableArrayEFM, ppe, temperatureFactor, ppeSpring, ppeFall, type){

  one <- basicClimateRating(ratingTableArrayMC,ppe,temperatureFactor,type)
  two <- climateModifyingFactors(ratingTableArrayESM,ratingTableArrayEFM, ppeSpring, ppeFall)
  return(climateRating(one,two))

}


#' Basic climate rating
#'
#' The basic climate rating is a designed to return the point deduction
#' for the moisture component and temperature factors. The max deduction is
#' taken for between the moisture factor and the temperature factor.
#' @param ratingTableArrayMC Rating table lower and upper bounds for deduction for the moisture component.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @param type If the crop uses effective growing degree days (EGDD) use EGDD else
#' if the crop uses crop heat units (CHU) use CHU.
#' @return Deduction points for the basic climate rating.
#' @export
basicClimateRating <- function(ratingTableArrayMC,ppe,temperatureFactor,type){

  moistureFactor <- moistureComponent(ratingTableArrayMC[1], ppe)

  # Need to determine if it's EGDD or CHU
  if(type == "EGDD"){
    temperatureFactor <- egddComponent(ratingTableArrayMC[2], temperatureFactor)
  } else if(type == "CHU"){
    temperatureFactor <- chuComponent(ratingTableArrayMC[2],temperatureFactor)
  } else {
    stop("Error determining if the crop uses effective growing degree days or
         crop heat units. Please specify EGDD or CHU")
  }

  return(min(moistureFactor,temperatureFactor))

}

#' Climate modifying factors
#'
#' The climate modifying factors is a designed to return the percentage deduction
#' for early spring moisture, excess fall moisture, and early fall frost. Max
#' deduction for each modifying factor is limited to 10% max modification.
#' @param ratingTableArrayESM Rating table lower and upper bounds for deduction for ESM.
#' @param ratingTableArrayEFM Rating table lower and upper bounds for deduction for EFM.
#' @param ppeSpring Precipitation minus potential evapotranspiration for spring
#' @param ppeFall Precipitation minus potential evapotranspiration for fall
#' @return
#' @export
climateModifyingFactors <- function(ratingTableArrayESM,ratingTableArrayEFM, ppeSpring, ppeFall){

  # Early spring moisture
  esm <- esmComponent(ratingTableArray, ppeSpring)
  # Excess fall moisture
  efm <- efmComponent(ratingTableArray, ppeFall)
  # Early fall frost
  # Not being used right now. Adding in future versions.
  # eff <- effComponent(inputArray, temperatureFall)

  return(sum(esm,efm))
  # Future versions use this
  # return(sum(esm,efm,eff))

}

#' Climate rating
#'
#' The climate rating calculates the rating class for the climate index.
#' @param basicClimate Basic climate rating calculated
#' @param modifiers Modifying factors.
#' @param ppeFall Precipitation minus potential evapotranspiration for fall
#' @return The climate rating.
#' @export
climateRating <- function(basicClimate,modifiers){

  # Basic climate rating is lower of moisture component and temperature factor.
  # The basicClimateRating function returns the minimum of the two so no further
  # calculations are required.
  a <- basicClimate
  # Modifiers is the percentage deduction for spring moisture, fall moisture and fall frost.
  # Each individual modifier should not exceed 10% deduction. The
  # climateModifyingFactors returns the sum of the modifiers. Divide by 100 to get %.
  b <- a * (modifiers / 100)
  # Climate rating
  rating <- (a - b)
  rating[rating < 0] <- 0
  rating[rating > 100] <- 100
  return(rating)

}
