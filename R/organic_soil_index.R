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
organicSoilIndexMain %<-% function(){

  one <- 1
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
