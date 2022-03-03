# Organic Soil index

# This library contains the organic soil index parameters.

# Creation date: Mar 02, 2022
# Last updated: Mar 02, 2022

#' Organic Soil Index Main
#'
#' The organic soil index main calls all required function and produces the rating
#' for organic soil over the study site.
#' @return
#' @export
organicSoilIndexMain <- function(egdd,
                                   ppe,surfaceBD,subsurfaceBD,depthToWaterTable,
                                   surfacepH,surfaceSalinity,
                                   subsurfacepH,subsurfaceSalinity){

  one <- organicBaseRating(egdd)
  two <- organicSoilMoistureDeduction(ppe,surfaceBD,subsurfaceBD,depthToWaterTable)
  three <- interimOrganicRating(surfaceBD, ppe, surfacepH,surfaceSalinity)
  four <- basicOrganicRating(subsurfaceBD,subsurfacepH,subsurfaceSalinity)
  five <- 0
  return(organicSoilRating(one,two,three,four,five))

}

#' Organic base rating (Z)
#'
#' The organic base rating looks at the micro climates of the organic soils.
#' Organic soils are commonly colder than those of associated mineral soils.
#' Because of these differences in soil thermal properties and because organic
#' soils usually occupy low positions in the landscape, a temperature deduction
#' is considered for organic soils.
#' @param egdd Effective growing degree days.
#' @return Return the organic base rating
#' @export
organicBaseRating <- function(egdd){
  if(is.na(egdd)){
    pointDeduct <- 0
  } else {
    if(egdd > 1600){
      pointDeduct <- 0
    } else if (egdd > 1200){
      pointDeduct <- (-0.05 * egdd) + 85
    } else {
      pointDeduct <- 25
    }
  }

  #Prevent negative deductions and deductions greater than 25 points.
  pointDeduct[pointDeduct < 0] <- 0
  pointDeduct[pointDeduct > 25] <- 25
  return(100 - pointDeduct)
}

#' Organic soil moisture deduction (M)
#'
#' The organic soil moisture deduction returns the point deduction for the water
#' supplying ability. The deduction is divided into two parts, the first being
#' the water holding capacity of the organic soil and the second being an adjustment
#' to part one, removing a percentage based on the water table depth.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param surfaceBD Surface bulk density
#' @param subsurfaceBD Subsurface bulk density
#' @param depthToWaterTable Depth to water table in cm
#' @return Organic soil moisture point deduction
#' @export
organicSoilMoistureDeduction <- function(ppe,surfaceBD,subsurfaceBD,depthToWaterTable){

  # 1. Water supplying ability
  # 1a. Surface water supplying ability
  if(is.na(surfaceBD) || is.na(ppe)){
    surfaceWSADFPointDeduct <- 0
  } else {
    surfaceWSA <- surfaceWSADF()
    bounds <- surfaceWSA[1,]
    if(surfaceBD < bounds[3]){
      tempcol <- 2
    } else if(surfaceBD < bounds[4]){
      tempcol <- 3
    } else if(surfaceBD < bounds[5]){
      tempcol <- 4
    } else if(surfaceBD < bounds[6]){
      tempcol <- 5
    } else if(surfaceBD < bounds[7]){
      tempcol <- 6
    } else {
      tempcol <- 7
    }
    tempcol <- surfaceWSA[,tempcol]
    bounds <- surfaceWSA[,1]
    if(ppe < bounds[2]){
      surfaceWSADFPointDeduct <- tempcol[2]
    } else if(ppe < bounds[3]){
      surfaceWSADFPointDeduct <- tempcol[3]
    } else if(ppe < bounds[4]){
      surfaceWSADFPointDeduct <- tempcol[4]
    } else if(ppe < bounds[5]){
      surfaceWSADFPointDeduct <- tempcol[5]
    } else if(ppe < bounds[6]){
      surfaceWSADFPointDeduct <- tempcol[6]
    } else if(ppe < bounds[7]){
      surfaceWSADFPointDeduct <- tempcol[7]
    } else if(ppe < bounds[8]){
      surfaceWSADFPointDeduct <- tempcol[8]
    } else {
      surfaceWSADFPointDeduct <- tempcol[9]
    }
  }
  # 1b. Subsurface water supplying ability based on depth to water table (cm)
  if(is.na(depthToWaterTable) || is.na(subsurfaceBD)){
    subsurfaceWSADFPointDeduct <- 0
  } else {
    subsurfaceWSA <- subsurfaceWSADF()
    bounds <- subsurfaceWSA[1,]
    if(subsurfaceBD < bounds[3]){
      tempcol <- 2
    } else if(subsurfaceBD < bounds[4]){
      tempcol <- 3
    } else if(subsurfaceBD < bounds[5]){
      tempcol <- 4
    } else if(subsurfaceBD < bounds[6]){
      tempcol <- 5
    } else if(subsurfaceBD < bounds[7]){
      tempcol <- 6
    } else if(subsurfaceBD < bounds[8]){
      tempcol <- 7
    } else if(subsurfaceBD < bounds[9]){
      tempcol <- 8
    } else {
      tempcol <- 9
    }
    tempcol <- subsurfaceWSA[,tempcol]
    bounds <- subsurfaceWSA[,1]
    if(depthToWaterTable < bounds[3]){
      subsurfaceWSADFPointDeduct <- tempcol[2]
    } else if(depthToWaterTable < bounds[4]){
      subsurfaceWSADFPointDeduct <- tempcol[3]
    } else if(depthToWaterTable < bounds[5]){
      subsurfaceWSADFPointDeduct <- tempcol[4]
    } else if(depthToWaterTable < bounds[6]){
      subsurfaceWSADFPointDeduct <- tempcol[5]
    } else if(depthToWaterTable < bounds[7]){
      subsurfaceWSADFPointDeduct <- tempcol[6]
    } else {
      subsurfaceWSADFPointDeduct <- tempcol[7]
    }
  }

  # 2. Return the deduction points for the moisture deduction
  return(surfaceWSADFPointDeduct - (surfaceWSADFPointDeduct * (subsurfaceWSADFPointDeduct / 100)))

}

#' Interim organic rating (surface factors)
#'
#' The interim organic rating returns the point deduction for the surface factors
#' of organic soils. The top 60cm of compacted peat is considered for the base
#' rating. Three factors are rated for their contribution to seed establishment,
#' crop growth and management. These are structure and consistence/degree of
#' decomposition (B) (fibre content), reaction (V) and nutrient status and
#' salinity (N).
#' @param surfaceBD Surface bulk density Mg/m^3
#' @param ppe Precipitation minus potential evapotranspiration
#' @param surfacepH Surface pH measured in saturated paste
#' @param surfaceSalinity Surface salinity measured in saturated paste (dS/m)
#' @return Point deduction for interim organic rating (surface factors).
#' @export
interimOrganicRating <- function(surfaceBD, ppe, surfacepH,surfaceSalinity){

  # 1. Structure and consistence (B)
  # This will change in future updates
  if(is.na(surfaceBD) || is.na(ppe)){
    BPointDeduct <- 0
  } else {
    OSCDF <- organicSCDF()
    bounds <- OSCDF[1,]
    if(surfaceBD < bounds[3]){
      tempcol <- 2
    } else if(surfaceBD < bounds[4]){
      tempcol <- 3
    } else if(surfaceBD < bounds[5]){
      tempcol <- 4
    } else if(surfaceBD < bounds[6]){
      tempcol <- 5
    } else if(surfaceBD < bounds[7]){
      tempcol <- 6
    } else if(surfaceBD < bounds[8]){
      tempcol <- 7
    } else if(surfaceBD < bounds[9]){
      tempcol <- 8
    } else {
      tempcol <- 9
    }
    tempcol <- OSCDF[,tempcol]
    bounds <- OSCDF[,1]
    if(ppe < bounds[2]){
      BPointDeduct <- tempcol[2]
    } else if(ppe < bounds[3]){
      BPointDeduct <- tempcol[3]
    } else if(ppe < bounds[4]){
      BPointDeduct <- tempcol[4]
    } else if(ppe < bounds[5]){
      BPointDeduct <- tempcol[5]
    } else if(ppe < bounds[6]){
      BPointDeduct <- tempcol[6]
    } else if(ppe < bounds[7]){
      BPointDeduct <- tempcol[7]
    } else {
      BPointDeduct <- tempcol[8]
    }
  }

  # 2.Reaction and nutrient status (V)
  if(is.na(surfaceBD) || is.na(surfacepH)){
    VPointDeduct <- 0
  } else {
    SORDF <- surfaceOrganicReactionDF()
    bounds <- SORDF[1,]
    if(surfaceBD < bounds[3]){
      tempcol <- 2
    } else if(surfaceBD < bounds[4]){
      tempcol <- 3
    } else if(surfaceBD < bounds[5]){
      tempcol <- 4
    } else {
      tempcol <- 5
    }
    tempcol <- SORDF[,tempcol]
    bounds <- SORDF[,1]
    if(surfacepH > bounds[2]){
      VPointDeduct <- tempcol[2]
    } else if(surfacepH > bounds[3]){
      VPointDeduct <- tempcol[3]
    } else if(surfacepH > bounds[4]){
      VPointDeduct <- tempcol[4]
    } else if(surfacepH > bounds[5]){
      VPointDeduct <- tempcol[5]
    } else if(surfacepH > bounds[6]){
      VPointDeduct <- tempcol[6]
    } else if(surfacepH > bounds[7]){
      VPointDeduct <- tempcol[7]
    } else if(surfacepH > bounds[8]){
      VPointDeduct <- tempcol[8]
    } else if(surfacepH > bounds[9]){
      VPointDeduct <- tempcol[9]
    } else if(surfacepH > bounds[10]){
      VPointDeduct <- tempcol[10]
    } else {
      VPointDeduct <- tempcol[11]
    }
  }

  # 3. Surface organic salinity (dS/m) (N)
  # This will change in the future to include user defined and crop specific
  # parameters.
  if(is.na(surfaceSalinity)){
    NPointDeduct <- 0
  } else {
    SOSDF <- surfaceOrganicSalinityDF()
    bounds <- SOSDF[,1]
    if(surfaceSalinity < bounds[1]){
      NPointDeduct <- SOSDF[1,2]
    } else if(surfaceSalinity < bounds[2]){
      NPointDeduct <- SOSDF[2,2]
    } else if(surfaceSalinity < bounds[3]){
      NPointDeduct <- SOSDF[3,2]
    } else {
      NPointDeduct <- SOSDF[4,2]
    }
  }

  # 4. Return the deduction points for the interim organic rating (surface factors)
  # Ensure that the value will not be over 100 or less than 0
  tempsum <- sum(BPointDeduct, VPointDeduct, NPointDeduct)
  tempsum[tempsum < 0] <- 1
  tempsum[tempsum > 100] <- 100
  return(tempsum)


}

#' Basic organic rating (subsurface factors)
#'
#' The basic organic rating (subsurface factors) are considered as modifiers
#' of the surface (base) rating. As such, the basic organic rating is a percentage
#' reduction. The maximum depth is considered at 120cm. There are four factors that
#' are recognized in this category, structure (degree of decomposition) (B), depth of
#' deposit and kind of substrate (G), reaction (V) and salinity (N). Depth of
#' deposit and kind of substrate are currently not being used.
#' @param
#' @param
#' @return
#' @export
basicOrganicRating <- function(subsurfaceBD,subsurfacepH,subsurfaceSalinity){

  # 1. Structure and consistence (B)
  if(is.na(subsurfaceBD)){
    BPercentDeduction <- 0
  } else {
    if(subsurfaceBD < 0.07){
      BPercentDeduct <- 20
    } else if(subsurfaceBD < 0.1){
      BPercentDeduct <- 10
    } else if(subsurfaceBD < 0.13){
      BPercentDeduct <- 0
    } else if(subsurfaceBD < 0.20){
      BPercentDeduct <- 5
    } else if(subsurfaceBD < 0.22){
      BPercentDeduct <- 10
    } else {
      BPrecentDeduct <- 20
    }
  }

  # 2. Depth of deposit and kind of substrate (G)
  # Currently not being used.

  # 3. Reaction and nutrient status (V)
  if(is.na(subsurfacepH)){
    VPercentDeduct <- 0
  } else {
    if(subsurfacepH > 5){
      VPercentDeduct <- 10
    } else if(subsurfacepH > 4){
      VPercentDeduct <- 20
    } else {
      VPercentDeduct <- 30
    }
  }

  # 4. Salinity (N)
  if(is.na(subsurfaceSalinity)){
    NPercentDeduct <- 0
  } else {
    if(subsurfaceSalinity < 4){
      NPercentDeduct <- 0
    } else if(subsurfaceSalinity < 8){
      NPercentDeduct <- 10
    } else {
      NPercentDeduct <- 20
    }
  }

  # 5. Return the percent deduction for basic organic rating (subsurface factors).
  # Ensure that the value will not be over 100 or less than 0
  tempsum <- sum(BPrecentDeduct, VPercentDeduct, NPercentDeduct)
  tempsum[tempsum < 0] <- 1
  tempsum[tempsum > 100] <- 100
  return(tempsum)

}

#' #' Drainage Deduction
#' #'
#' #' The drainage deduction is used to evaluate the soil properties which include
#' #' the water table and hydraulic conductivity.The rating is based principally on
#' #' management or traffic ability considerations. Three is one parameter for
#' #' drainage. This parameter determines the percentage deduction for the soil
#' #' regime. Currently this parameter is not used in the calculations with potential
#' #' for future version to include the drainage.
#' #' @param depthToWaterTable Depth to water table in cm (Highest 20-day average in
#' #' growing season).
#' #' @param ppe Precipitation minus potential evapotranspiration.
#' #' @param hydraulicCond Hydraulic conductivity (cm/h)
#' #' @return Percentage deduction for drainage.
#' #' @export
#' drainageDeduction <- function(depthToWaterTable,ppe,hydraulicCond){
#'   # 2. Return the deduction percentage for the drainage deduction
#'   return()
#'
#' }

#' Organic Soil rating
#'
#' The organic soil rating calculates the rating class for the organic soil index.
#' @param basicClimate Basic climate rating calculated
#' @param modifiers Modifying factors.
#' @param ppeFall Precipitation minus potential evapotranspiration for fall
#' @return The climate rating.
#' @export
organicSoilRating <- function(soilTemp,moistureDeduct,surfaceFactors,subsurfaceFactors,drainage){

  # Soil climate factor (Z)
  z <- soilTemp
  # Moisture deduct is the moisture factor for the organic soils.
  d <- moistureDeduct
  # Surface factors is the interim soil rating for the organic soils.
  e <- surfaceFactors
  f <- z - d - e
  # Subsurface factors is the basic soil rating for the organic soils.
  g <- f  * (subsurfaceFactors/100)
  h <- f - g
  # Drainage is the drainage factors for the organic soils. Currently not being used.
  i <- h * (drainage/100)
  # organic soil rating
  rating <- (h - i)
  rating[rating < 0] <- 1
  rating[rating > 100] <- 100
  return(rating)
}
