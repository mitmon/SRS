# Default data

# This R file contains the default data for data tables along with default inputs.

# Creation date: Mar 01, 2022
# Last updated: Mar 01, 2022


#' Surface available water holding capacity data table
#'
#' The available water holding capacity (texture) aids in the determination of
#' the soils ability to retain and supply water to plants.
#' @return The available water holding capacity data table.
#' @export
surfaceAWHCDF <- function(){
  # First column is the P-PE for the AWHCDF
  return(cbind(c(0,0,-50,-100,-150,-200,-250,-300,-350,-400,-450,-500),
        c(0,40,50,60,70,80,90,95,95,95,95,95),
        c(10,15,25,35,45,55,65,75,85,90,95,95),
        c(20,0,0,10,20,35,50,60,70,80,90,95),
        c(40,0,0,0,10,20,35,50,60,70,80,90),
        c(60,0,0,0,0,10,25,40,50,60,70,80),
        c(70,0,0,0,0,10,20,30,45,55,65,75),
        c(75,0,0,0,0,10,20,30,45,55,65,75),
        c(80,0,0,0,0,10,20,30,40,50,60,70),
        c(85,0,0,0,0,10,20,30,40,50,60,70),
        c(95,0,0,0,0,10,20,30,40,50,60,70)))
}

#' Subsurface available water holding capacity data table
#'
#' The available water holding capacity (texture) aids in the determination of
#' the soils ability to retain and supply water to plants.
#' @return The available water holding capacity data table.
#' @export
subsurfaceAWHCDF <- function(){
  # First column is the average subsurface texture % clay + silt
  return(cbind(c(0,10,20,40,60,70),
               c(10,0,-5,-15,-25,-30),
               c(20,5,0,-5,-10,-20),
               c(40,15,5,0,-5,-15),
               c(60,25,15,5,0,-5),
               c(70,30,20,10,5,0)))
}

#' Depth of water table adjustments data table
#'
#' The depth of the water table effects the moisture supplying ability within
#' the root zone.
#' @return The water table adjustments
#' @export
waterTableAdjustmentDF <- function(){
  # First column is the depth of the water table (cm)
  # First row is the texture/drainage class
  return(cbind(c(0,0,25,50,75,100,125),
               c(20,100,90,60,20,0,0),
               c(70,100,90,70,40,0,0),
               c(100,100,90,75,50,20,0)))
}

#' Organic matter content data table
#'
#' The organic carbon percentage data table.
#' @return Organic matter content data table.
#' @export
OMDTDF <- function(){
  # First column is the organic carbon %
  return(cbind(c(4,3,2,1,0.5),
               c(0,2,5,10,15)))
}

#' Depth of topsoil data table
#'
#' The depth of topsoil data table.
#' @return Depth of topsoil data table (cm)
#' @export
depthOfTopSoilDF <- function(){
  # First column is the depth of the topsoil (cm)
  return(cbind(c(20,15,10,5,0),
               c(0,5,10,15,20)))
}

#' Surface reaction (pH) data table
#'
#' The surface reaction (pH) data table
#' @return Surface reaction (pH) data table
#' @export
surfaceReactionDF <- function(){
  # First column is the soil pH
  return(cbind(c(9.0,8.5,6,5.5,5.0,4.5,4.0,3.5),
               c(60,20,5,0,5,15,30,55,80)))
}

#' Surface salinity data table
#'
#' The surface salinity (dS/m) data table
#' @return Surface salinity (dS/m) data table
#' @export
surfaceSalinityDF <- function(){
  # First column is the soil salinity
  return(cbind(c(2,4,8,16),
               c(0,20,50,90)))
}

#' Surface sodicity data frame
#'
#' The surface sodicity (sodium adsorption ratio) data frame
#' @return surface sodicity (sodium adsorption ratio) data frame
#' @export
surfaceSodicityDF <- function(){
  # First column is the soil sodicity
  return(cbind(c(4,8,12,16,20),
               c(60,80,120,160,160),
               c(0,10,30,50,80)))
}

#' Subsurface structure and consistence data frame
#'
#' The subsurface structure and consistence data frame.
#' @return The subsurface structure and consistence data frame.
#' @export
subsurfaceSCDF <- function(){
  # First column is the bulk density
  return(cbind(c(0,1.20,1.30,1.35,1.40,1.45,1.50,1.60,1.70,1.80,1.90,2.00),
               c(0,0,0,0,0,0,0,10,30,50,70,80),
               c(10,0,0,0,0,0,5,20,40,60,80,90),
               c(20,0,0,0,0,5,10,30,50,70,90,90),
               c(35,0,0,0,5,10,20,40,60,80,90,90),
               c(50,0,0,5,10,20,40,55,70,90,90,90),
               c(70,0,5,10,20,40,50,70,90,90,90,90)))
}

#' Modifications for the impeding subsurface layers data frame
#'
#' The modifications for the impeding subsurface layers data frame.
#' @return The modifications for the impeding subsurface layers data frame.
#' @export
subsurfaceImpedingDF <- function(){
  # First column is the depth (cm) for impeding subsurface layers in different
  # climate zones.
  return(cbind(c(0,15,30,60,100,200),
               c(-50,90,70,40,0,0),
               c(-150,95,75,50,0,0),
               c(-250,100,80,60,10,0)))
}

#' Water supplying ability data frame
#'
#' The water supplying ability aid determines the soils ability to retain and supply water to plants.
#' @return The water supplying ability data frame
#' @export
surfaceWSADF <- function(){
  # First column is the precipitation minus potential evapotranspiration
  return(cbind(c(0,-250,-200,-150,-100,-50,0,50),
               c(0.04,40,35,30,25,20,15,5),
               c(0.07,30,25,20,15,10,5,0),
               c(0.10,20,15,10,5,0,0,0),
               c(0.13,15,10,5,0,0,0,0),
               c(0.16,10,5,0,0,0,0,0),
               c(0.18,5,0,0,0,0,0,0)))
}

#' Water supplying ability data frame
#'
#' The subsurface water supplying ability aid determines the soils ability to retain and supply water to plants.
#' @return The subsurface water supplying ability data frame
#' @export
subsurfaceWSADF <- function(){
  # First column is the subsurface bulk density
  return(cbind(c(0,0,25,50,75,100,125),
               c(0.04,100,90,60,20,0,0),
               c(0.07,100,90,70,30,0,0),
               c(0.10,100,95,80,40,10,0),
               c(0.13,100,95,80,50,20,0),
               c(0.16,100,95,85,60,30,10),
               c(0.18,100,100,90,70,45,20),
               c(0.20,100,100,95,80,60,30),
               c(0.22,100,100,95,90,70,50)))
}
