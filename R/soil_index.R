# Soil index

# This library contains the soil index parameters.

# Creation date: Feb 23, 2022
# Last updated: Mar 01, 2022

#' Soil Moisture deduction
#'
#' The moisture deduction returns the point deduction for the available water
#' holding capacity "AWHC", the surface texture, subsurface texture, and water
#' table depth. The deduction is divided into two part, the first being the
#' texture deduction using the AWHC, surface and subsurface textures. The second
#' part takes a removes a percentage of the deduction based on the water table
#' depth.
#' @param ppe Precipitation minus potential evapotranspiration
#' @param surfaceSiltPercent Percentage of surface (depths 0-60cm) silt
#' @param surfaceClayPercent Percentage of surface (depths 0-60cm) clay
#' @param subsurfaceSiltPercent Percentage of subsurface (depths 61-200cm) silt
#' @param subsurfaceClayPercent Percentage of subsurface (depths 61-200cm) clay
#' @param waterTableDepth Water table depth (in cm)
#' @return Deduction points for the moisture deduction.
#' @export
soilMoistureDeduction <- function(ppe,surfaceSiltPercent,surfaceClayPercent,subsurfaceSiltPercent,subsurfaceClayPercent,waterTableDepth){

  # 1. Available water holding capacity
  # 1a. Surface available water holding capacity
  if(is.na(surfaceSiltPercent) || is.na(surfaceClayPercent)){
    surfaceAWHCDFPointDeduct <- 0
  } else {
    texture <- soilTexture(surfaceSiltPercent,surfaceClayPercent)
    AWHCDF <- surfaceAWHCDF()
    bounds <- AWHCDF[1,]
    for(i in 1:length(bounds)){
      if(texture < bounds[3]){
        tempcol <- 2
      } else if(texture < bounds[4]){
        tempcol <- 3
      } else if(texture < bounds[5]){
        tempcol <- 4
      } else if(texture < bounds[6]){
        tempcol <- 5
      } else if(texture < bounds[7]){
        tempcol <- 6
      } else if(texture < bounds[8]){
        tempcol <- 7
      } else if(texture < bounds[9]){
        tempcol <- 8
      } else if(texture < bounds[10]){
        tempcol <- 9
      } else if(texture < bounds[11]){
        tempcol <- 10
      } else {
        tempcol <- 11
      }
    }
    tempcol <- AWHCDF[,tempcol]
    bounds <- AWHCDF[,1]
    for(i in 1:length(AWHCDF)){
      if(ppe > bounds[2]){
        surfaceAWHCDFPointDeduct <- tempcol[1]
      } else if(ppe > bounds[3]){
        surfaceAWHCDFPointDeduct <- tempcol[2]
      } else if(ppe > bounds[4]){
        surfaceAWHCDFPointDeduct <- tempcol[3]
      } else if(ppe > bounds[5]){
        surfaceAWHCDFPointDeduct <- tempcol[4]
      } else if(ppe > bounds[6]){
        surfaceAWHCDFPointDeduct <- tempcol[5]
      } else if(ppe > bounds[7]){
        surfaceAWHCDFPointDeduct <- tempcol[6]
      } else if(ppe > bounds[8]){
        surfaceAWHCDFPointDeduct <- tempcol[7]
      } else if(ppe > bounds[9]){
        surfaceAWHCDFPointDeduct <- tempcol[8]
      } else if(ppe > bounds[10]){
        surfaceAWHCDFPointDeduct <- tempcol[9]
      } else if(ppe > bounds[11]){
        surfaceAWHCDFPointDeduct <- tempcol[10]
      } else if(ppe > bounds[12]){
        surfaceAWHCDFPointDeduct <- tempcol[11]
      } else {
        surfaceAWHCDFPointDeduct <- tempcol[12]
      }
    }
  }

  # 1b. Subsurface available water holding capacity
  if(is.na(subsurfaceSiltPercent) || is.na(subsurfaceClayPercent)){
    subsurfaceAWHCDFPointDeduct <- 0
  } else {
    textureminus <- surfaceSiltPercent - surfaceClayPercent
    textureSub <- soilTexture(subsurfaceSiltPercent,subsurfaceClayPercent)
    AWHCDF <- subsurfaceAWHCDF()
    bounds <- AWHCDF[1,]
    for(i in 1:length(bounds)){
      if(textureminus < bounds[3]){
        tempcol <- 2
      } else if(textureminus < bounds[4]){
        tempcol <- 3
      } else if(textureminus < bounds[5]){
        tempcol <- 4
      } else if(textureminus < bounds[6]){
        tempcol <- 5
      } else {
        tempcol <- 6
      }
    }
    tempcol <- AWHCDF[,tempcol]
    bounds <- AWHCDF[,1]
    for(i in 1:length(AWHCDF)){
      if(textureSub < bounds[3]){
        subsurfaceAWHCDFPointDeduct <- tempcol[2]
      } else if(textureSub < bounds[4]){
        subsurfaceAWHCDFPointDeduct <- tempcol[3]
      } else if(textureSub < bounds[5]){
        subsurfaceAWHCDFPointDeduct <- tempcol[4]
      } else if(textureSub < bounds[6]){
        subsurfaceAWHCDFPointDeduct <- tempcol[5]
      } else {
        subsurfaceAWHCDFPointDeduct <- tempcol[6]
      }
    }
  }

  # 2. Water table adjustments
  if(is.na(waterTableDepth)){
    WTAPointDeduct <- 0
  } else {
    WTA <- waterTableAdjustmentDF()
    bounds <- WTA[1,]
    for(i in 1:length(bounds)){
      if(texture < bounds[3]){
        tempcol <- 2
      } else if(texture < bounds[4]){
        tempcol <- 3
      } else {
        tempcol <- 4
      }
    }
    tempcol <- WTA[,tempcol]
    bounds <- WTA[,1]
    for(i in 1:length(bounds)){
      if(waterTableDepth < bounds[3]){
        WTAPointDeduct <- tempcol[2]
      } else if(waterTableDepth < bounds[4]){
        WTAPointDeduct <- tempcol[3]
      } else if(waterTableDepth < bounds[5]){
        WTAPointDeduct <- tempcol[4]
      } else if(waterTableDepth < bounds[6]){
        WTAPointDeduct <- tempcol[5]
      } else if(waterTableDepth < bounds[7]){
        WTAPointDeduct <- tempcol[6]
      } else {
        WTAPointDeduct <- tempcol[7]
      }
    }
  }

  # 3. Return the deduction points for the moisture deduction
  return(sum(surfaceAWHCDFPointDeduct,subsurfaceAWHCDFPointDeduct) + (sum(surfaceAWHCDFPointDeduct,subsurfaceAWHCDFPointDeduct) * (WTAPointDeduct) / 100))

}

#' Interim soil rating (surface factors)
#'
#' The interim soil rating determines a point deduction based on the surface
#' parameters of the soil. There are seven parameters for surface factors. These
#' are structure and consistence (D), organic matter context (colour) (F),
#' depth of top soil (E), reaction (pH) (V), salinity (dS/m) (N), sodicity (SAR) (Y),
#' and peat depth (cm) (O). Currently, E, N, Y, and O are not used. Future updates
#' will include this additional information.
#' @param surfaceSiltPercent Percentage of surface (depths 0-60cm) silt
#' @param surfaceClayPercent Percentage of surface (depths 0-60cm) clay
#' @param surfaceOC Percentage of surface (depths 0-60cm) organic carbon
#' @param depthOfTopSoil Depth of top soil up to a max of 20cm
#' @param surfacepH Surface pH measured in saturated paste (depths 0-60cm)
#' @param surfaceSalinity Surface salinity (depths 0-60cm)
#' @param surfaceSodicity Surface sodicity (depths 0-60cm)
#' @param depthOfPeat Depth of organic (peaty) surface
#' @return Deduction points for interim soil rating.
#' @export
interimSoilRating <- function(surfaceSiltPercent,surfaceClayPercent,surfaceOC,depthOfTopSoil,surfacepH,surfaceSalinity,surfaceSodicity,depthOfPeat){

  # 1. Structure and consistence (D)
  # This will change in future updates
  if(is.na(surfaceSiltPercent) || is.na(surfaceClayPercent) || is.na(surfaceOC)){
    DPointDeduct <- 0
  } else {
    surfaceSandPercent <- 100 - surfaceSiltPercent - surfaceClayPercent
    DPointDeduct <- surfaceOC
    DPointDeduct[surfaceOC>3] <- 0
    DPointDeduct[surfaceOC<=3] <- (3 / surfaceOC[surfaceOC<3]) + ((surfaceSandPercent[surfaceOC<3]) / 3 * surfaceOC[surfaceOC<3]) + surfaceSiltPercent[surfaceOC<3]

    #Prevent negative deductions and deductions greater than 10 points.
    DPointDeduct[DPointDeduct<0] <- 0
    DPointDeduct[DPointDeduct>10] <- 10
  }

  # 2. Organic matter content (F)
  if(is.na(surfaceOC)){
    FPointDeduct <- 0
  } else {
    OMDF <- OMDTDF()
    bounds <- OMDF[1,]
    if(surfaceOC > bounds[1]){
      FPointDeduct <- OMDF[1,2]
    } else if(surfaceOC > bounds[2]){
      FPointDeduct <- OMDF[2,2]
    } else if(surfaceOC > bounds[3]){
      FPointDeduct <- OMDF[3,2]
    } else if(surfaceOC > bounds[4]){
      FPointDeduct <- OMDF[4,2]
    } else {
      FPointDeduct <- OMDF[5,2]
    }
  }

  # 3. Depth of topsoil (E)
  if(is.na(depthOfTopSoil)){
    EPointDeduct <- 0
  } else {
    DTSDF <- depthOfTopSoilDF()
    bounds <- DTSDF[1,]
    if(depthOfTopSoil > bounds[1]){
      EPointDeduct <- DTSDF[1,2]
    } else if(depthOfTopSoil > bounds[2]){
      EPointDeduct <- DTSDF[2,2]
    } else if(depthOfTopSoil > bounds[3]){
      EPointDeduct <- DTSDF[3,2]
    } else if(depthOfTopSoil > bounds[4]){
      EPointDeduct <- DTSDF[4,2]
    } else {
      EPointDeduct <- DTSDF[5,2]
    }
  }

  # 4. Surface reaction (pH) (V)
  if(is.na(surfacepH)){
    VPointDeduct <- 0
  } else {
    SRDF <- surfaceReactionDF()
    bounds <- SRDF[1,]
    if(surfacepH > bounds[1]){
      VPointDeduct <- SRDF[1,2]
    } else if(surfacepH > bounds[2]){
      VPointDeduct <- SRDF[2,2]
    } else if(surfacepH > bounds[3]){
      VPointDeduct <- SRDF[3,2]
    } else if(surfacepH > bounds[4]){
      VPointDeduct <- SRDF[4,2]
    } else if(surfacepH > bounds[5]){
      VPointDeduct <- SRDF[5,2]
    } else if(surfacepH > bounds[6]){
      VPointDeduct <- SRDF[6,2]
    } else if(surfacepH > bounds[7]){
      VPointDeduct <- SRDF[7,2]
    } else if(surfacepH > bounds[8]){
      VPointDeduct <- SRDF[8,2]
    } else {
      VPointDeduct <- SRDF[9,2]
    }
  }

  # 5. Surface salinity (dS/m) (N)
  # This will change in the future to include user defined and crop specific
  # parameters.
  if(is.na(surfaceSalinity)){
    NPointDeduct <- 0
  } else {
    SSDF <- surfaceSalinityDF()
    bounds <- SSDF[1,]
    if(surfaceSalinity < bounds[1]){
      NPointDeduct <- SSDF[1,2]
    } else if(surfaceSalinity < bounds[2]){
      NPointDeduct <- SSDF[2,2]
    } else if(surfaceSalinity < bounds[3]){
      NPointDeduct <- SSDF[3,2]
    } else {
      NPointDeduct <- SSDF[4,2]
    }
  }

  # 6. Surface sodicity (sodium adsorption ratio) (Y)
  if(is.na(surfaceSodicity)){
    YPointDeduct <- 0
  } else {
    SSDF <- surfaceSodicityDF()
    bounds <- SSDF[1,]
    if(surfaceSodicity < bounds[1]){
      YPointDeduct <- SSDF[1,3]
    } else if(surfaceSodicity < bounds[2]){
      YPointDeduct <- SSDF[2,3]
    } else if(surfaceSodicity < bounds[3]){
      YPointDeduct <- SSDF[3,3]
    } else if(surfaceSodicity < bounds[4]){
      YPointDeduct <- SSDF[5,3]
    } else {
      YPointDeduct <- SSDF[6,3]
    }
  }

  # 7. Depth of peat (cm) (O)
  # if(is.na(depthOfPeat)){
  #   OPointDeduct <- 0
  # } else {
  #   if(depthOfPeat >= 40){
  #     OPointDeduct <- ((depthOfPeat - 10) * (sqrt(0.12))) / sqrt(bulkDensity)
  #   } else {
  #     OPointDeduct <- 0
  #   }
  # }

  # 8. Return the deduction points for the interim soil rating (surface factors)
  return(100 - sum(DPointDeduct,FPointDeduct,VPointDeduct))

}

#' Basic soil rating (subsurface factors)
#'
#' The basic soil rating determines a point deduction based on the subsurface
#' parameters of the soil. There are five parameters for subsurface factors. These
#' are impeding layer (structure and consistence) (D), contrasting texture,
#' reaction (pH) (V), salinity (EC) (N), sodicity (SAR) (Y). Currently, E, N, Y,
#' and O are not used. Future updates will include this additional information.
#' @param subsurfaceSiltPercent Percentage of subsurface (depths 60-200cm) silt
#' @param subsurfaceClayPercent Percentage of subsurface (depths 60-200cm) clay
#' @param subsurfaceBulkDensity Subsurface bulk density
#' @param impedingLayerDepth Subsurface impeding layers depth (cm)
#' @param subsurfaceSalinity Subsurface salinity (depths 60-200cm)
#' @param subsurfaceSodicity Subsurface sodicity (depths 60-200cm)
#' @return Deduction points for basic soil rating.
#' @export
basicSoilRating <- function(subsurfaceSiltPercent,subsurfaceClayPercent,subsurfaceBulkDensity,impedingLayerDepth,subsurfacepH,subsurfaceSalinity,subsurfaceSodicity){

  # 1. Structure and consistence (D)
  # This will change in future updates
  if(is.na(subsurfaceSiltPercent) || is.na(subsurfaceClayPercent) || is.na(subsurfaceBulkDensity)){
    DPointDeduct <- 0
  } else {
    surfaceSandPercent <- 100 - surfaceSiltPercent - surfaceClayPercent
    DPointDeduct <- surfaceOC
    DPointDeduct[surfaceOC>3] <- 0
    DPointDeduct[surfaceOC<=3] <- (3 / surfaceOC[surfaceOC<3]) + ((surfaceSandPercent[surfaceOC<3]) / 3 * surfaceOC[surfaceOC<3]) + surfaceSiltPercent[surfaceOC<3]

    #Prevent negative deductions and deductions greater than 10 points.
    DPointDeduct[DPointDeduct<0] <- 0
    DPointDeduct[DPointDeduct>10] <- 10
  }

  # 7. Return the deduction points for the interim soil rating (surface factors)
  return(100 - sum(DPointDeduct,FPointDeduct,VPointDeduct))

}

