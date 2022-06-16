# Climate index

# This library contains the climate index parameters.

# Creation date: Feb 23, 2022
# Last updated: Mar 02, 2022

#' Climate Index Main
#'
#' The climate index main calls all required function and produces the rating
#' for climate over the study site.
#' @param ratingTableArrayMC Rating table lower and upper bounds for deduction for the moisture component.
#' @param ratingTableArrayTF Rating table lower and upper bounds for deduction for temperature factors.
#' @param ratingTableArrayESM Rating table lower and upper bounds for deduction for early spring moisture.
#' @param ratingTableArrayEFM Rating table lower and upper bounds for deduction for excess fall moisture.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat
#' units for the study site.
#' @param ppeSpring Precipitation minus potential evapotranspiration for spring/April
#' @param ppeFall Precipitation minus potential evapotranspiration for fall/October
#' @param type If the crop uses effective growing degree days (EGDD) use EGDD else
#' if the crop uses crop heat units (CHU) use CHU.
#' @return Deduction points for the basic climate rating.
#' @export
climateIndexMain <- function(ratingTableArrayMC,ratingTableArrayTF,ratingTableArrayESM,ratingTableArrayEFM, ppe, temperatureFactor, ppeSpring, ppeFall, type){

  # oneA <- mapply(moistureFactorRating,ratingTableArrayMC,ppe)
  # oneb <- mapply(temperatureFactorRating,ratingTableArrayTF,ppe,temperatureFactor,type)
  # two <- mapply(climateModifyingFactors,ratingTableArrayESM,ratingTableArrayEFM, ppeSpring, ppeFall)
  # results <- mapply(climateRating, oneA, oneB, two)
  # return(results)

  one <- mapply(basicClimateRating,ratingTableArrayMC,ratingTableArrayTF,ppe,temperatureFactor,type)
  two <- mapply(climateModifyingFactors,ratingTableArrayESM,ratingTableArrayEFM, ppeSpring, ppeFall)
  results <- mapply(climateRating, one, two)
  return(results)


}

#' Basic climate rating
#'
#' The basic climate rating is a designed to return the point deduction
#' for the moisture component and temperature factors. The max deduction is
#' taken for between the moisture factor and the temperature factor.
#' @param ratingTableArrayMC Rating table lower and upper bounds for deduction for the moisture component.
#' @param ratingTableArrayTF Rating table lower and upper bounds for deduction for temperature factors.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @param type If the crop uses effective growing degree days (EGDD) use EGDD else
#' if the crop uses crop heat units (CHU) use CHU.
#' @return Deduction points for the basic climate rating.
#' @export
basicClimateRating <- function(ratingTableArrayMC,ratingTableArrayTF,ppe,temperatureFactor,type){

  moistureFactor <- moistureComponent(ratingTableArrayMC, ppe)

  # Need to determine if it's EGDD or CHU
  if(type == "EGDD"){
    temperatureFactor <- egddComponent(ratingTableArrayTF, temperatureFactor)
  } else if(type == "CHU"){
    temperatureFactor <- chuComponent(ratingTableArrayTF,temperatureFactor)
  } else {
    stop("Error determining if the crop uses effective growing degree days or
         crop heat units. Please specify EGDD or CHU")
  }

  return((100 - min(moistureFactor,temperatureFactor)))

}

#' Moisture factor rating
#'
#' @param ratingTableArrayMC Rating table lower and upper bounds for deduction for the moisture component.
#' @param ppe Precipitation minus potential evapotranspiration
#' @return Deduction points for the basic climate rating.
#' @export
moistureFactorRating <- function(ratingTableArrayMC,ppe){

  moistureFactor <- moistureComponent(ratingTableArrayMC, ppe)

  return(moistureFactor)

}

#' Temperature Factor rating
#'
#' @param ratingTableArrayTF Rating table lower and upper bounds for deduction for temperature factors.
#' @param temperatureFactor Input effective growing degree days or crop heat units for
#' the study site.
#' @param type If the crop uses effective growing degree days (EGDD) use EGDD else
#' if the crop uses crop heat units (CHU) use CHU.
#' @return Deduction points for the basic climate rating.
#' @export
temperatureFactorRating <- function(ratingTableArrayTF,temperatureFactor,type){

  # Need to determine if it's EGDD or CHU
  if(type == "EGDD"){
    temperatureFactor <- egddComponent(ratingTableArrayTF, temperatureFactor)
  } else if(type == "CHU"){
    temperatureFactor <- chuComponent(ratingTableArrayTF,temperatureFactor)
  } else {
    stop("Error determining if the crop uses effective growing degree days or
         crop heat units. Please specify EGDD or CHU")
  }

  return(temperatureFactor)

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
  esm <- esmComponent(ratingTableArrayESM, ppeSpring)
  # Excess fall moisture
  efm <- efmComponent(ratingTableArrayEFM, ppeFall)
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
#' @return The climate rating.
#' @export
climateRating <- function(climateFactor, modifiers){

  # Basic climate rating is lower of moisture component and temperature factor.
  # The basicClimateRating function returns the minimum of the two so no further
  # calculations are required.
  a <- climateFactor
  # Modifiers is the percentage deduction for spring moisture, fall moisture and fall frost.
  # Each individual modifier should not exceed 10% deduction. The
  # climateModifyingFactors returns the sum of the modifiers. Divide by 100 to get %.
  b <- a * (modifiers / 100)
  # Climate rating
  rating <- (a - b)
  ## Dev tools ##

  # rating <- ratingTable(rating) * 100
  # a <- ratingTable(a) * 10
  # b <- ratingTable(b) * 1
  # rating <- rating + a + b

  ##
  # rating[rating < 0] <- 0
  # rating[rating > 100] <- 100

  return(rating)

}
