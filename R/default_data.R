# Default data

# This R file contains the default data for data tables along with default inputs.

# Creation date: Mar 01, 2022
# Last updated: Jun 02, 2022

#' Alfalfa default data table
#'
#' The alfalfa default data table holds pre-defined data if there is no user
#' input data.
#' @return Alfalfa default data
#' @export
alfalfaDefaultDF <- function(){
  return(rbind(
    list(list(-150,0,0),list(-300,-150,30),list(-400,-300,50),list(-500,-400,70),list(-550,-500,80),list(-600,-550,90),list(-650,-600,100)),  # Moisture deduction
    list(list(1890,2200,0),list(1410,1890,20),list(930,1410,55),list(480,930,70),list(0,480,80),list(NA),list(NA)), # Effective growing degree days T5
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture

}

#' Canola default data table
#'
#' The Canola default data table holds pre-defined data if there is no user
#' input data.
#' @return Canola default data
#' @export
canolaDefaultDF <- function(){
  return(rbind(
    list(list(-150,0,0),list(-300,-150,30),list(-400,-300,50),list(-500,-400,70),list(-550,-500,80),list(-600,-550,90),list(-650,-600,100)), # Moisture deduction
    list(list(1200,2000,0),list(1050,1200,40),list(900,1050,55),list(500,900,70),list(0,500,90),list(NA),list(NA)), # Effective growing degree days T5
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture

}

#' Corn default data table
#'
#' The corn default data table holds pre-defined data if there is no user
#' input data.
#' @return Corn default data
#' @export
cornDefaultDF <- function(){
  return(rbind(
    list(list(-50,0,0),list(-150,-50,21),list(-250,-150,41),list(-300,-250,70),list(-400,-300,80),list(-400,-550,90),list(-550,-400,100)), # Moisture deduction
    list(list(2700,3500,0),list(2300,2700,21),list(2000,2300,55),list(1700,2000,70),list(1200,1700,80),list(800,1200,90),list(0,800,100)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Potato default data table
#'
#' The Potato default data table holds pre-defined data if there is no user
#' input data.
#' @return Potato default data
#' @export
potatoDefaultDF <- function(){
  return(rbind(
    list(list(-150,0,0),list(-300,-150,30),list(-400,-300,50),list(-500,-400,70),list(-550,-500,80),list(-600,-550,90),list(-650,-600,100)), # Moisture deduction
    list(list(2700,3500,0),list(2300,2700,40),list(2000,2300,55),list(1200,2000,70),list(0,1200,90),list(NA),list(NA)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Soybean default data table
#'
#' The soybean default data table holds pre-defined data if there is no user
#' input data.
#' @return Soybean default data
#' @export
soybeanDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(2700,7000,0),list(2300,2700,40),list(2000,2300,55),list(1700,2000,70),list(1200,1700,80),list(0,1200,90),list(NA)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Spring seeded small grain (SSSG) default data table
#'
#' The spring seeded small grain default data table holds pre-defined data if there is no user
#' input data.
#' @return Spring seeded small grain default data
#' @export
SSSGDefaultDF <- function(){
  return(rbind(
    list(list(-150,0,0),list(-300,-150,30),list(-400,-300,50),list(-500,-400,70),list(-600,-500,80),list(-15000,-600,100),list(NA)), # Moisture deduction
    list(list(1200,7000,0),list(1050,1200,40),list(900,1050,55),list(500,900,70),list(0,500,90),list(NA),list(NA)), # Effective growing degree days T5
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Peas default data table
#'
#' The peas default data table holds pre-defined data if there is no user
#' input data.
#' @return peas default data
#' @export
peasDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(1527,7000,0),list(1368,1527,20),list(1209,1368,40),list(1050,1209,55),list(891,1050,70),list(732,891,80),list(0,732,90)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Lentils default data table
#'
#' The Lentils default data table holds pre-defined data if there is no user
#' input data.
#' @return Lentils default data
#' @export
lentilsDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(1740,7000,0),list(1604,1740,20),list(1468,1604,40),list(1332,1468,55),list(1196,1332,70),list(1060,1196,80),list(0,1060,90)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Dry beans default data table
#'
#' The dry beans default data table holds pre-defined data if there is no user
#' input data.
#' @return dry beans default data
#' @export
drybeansDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(1100,7000,0),list(900,1100,20),list(700,900,40),list(500,700,55),list(300,500,70),list(NA),list(NA)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Faba beans default data table
#'
#' The faba beans default data table holds pre-defined data if there is no user
#' input data.
#' @return faba beans default data
#' @export
fababeansDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(2700,7000,0),list(2300,2700,40),list(2000,2300,55),list(1700,2000,70),list(1200,1700,80),list(0,1200,90),list(NA)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}

#' Chickpeas default data table
#'
#' The chickpeas default data table holds pre-defined data if there is no user
#' input data.
#' @return chickpeas default data
#' @export
chickpeasDefaultDF <- function(){
  return(rbind(
    list(list(-100,0,0),list(-200,-100,20),list(-300,-200,40),list(-450,-300,70),list(-600,-450,100),list(NA),list(NA)), # Moisture deduction
    list(list(1679,7000,0),list(1555,1679,20),list(1431,1555,40),list(1307,1431,55),list(1183,1307,70),list(1059,1183,80),list(0,1059,90)), # Crop heat units
    list(list(-50,-40,0),list(-40,-10,2.5),list(-10,10,5),list(10,30,7.5),list(30,50,10),list(NA),list(NA)), # Excess spring moisture
    list(list(0,20,2),list(20,40,4),list(40,60,6),list(60,80,8),list(80,100,10),list(NA),list(NA)))) # Early fall moisture
}



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

#' Surface mineral reaction (pH) data table
#'
#' The surface reaction (pH) data table
#' @return Surface reaction (pH) data table
#' @export
surfaceReactionDF <- function(){
  # First column is the soil pH
  return(cbind(c(9.0,8.5,6,5.5,5.0,4.5,4.0,3.5),
               c(60,20,5,0,5,15,30,55,80)))
}

#' Surface mineral salinity data table
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
#' The subsurface water supplying ability helps determine the soils ability to retain and supply water to plants.
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

#' Organic structure and consistence data frame
#'
#' The organic structure and consistence data frame helps determine the preparation
#' of a proper seedbed.
#' @return The organic structure and consistence data frame
#' @export
organicSCDF <- function(){
  # First column is the precipitation minus potential evapotranspiration
  return(cbind(c(0,-250,-200,-150,-100,-50,0,50),
               c(0.04,60,50,40,30,25,20,15),
               c(0.07,50,40,30,25,15,10,5),
               c(0.10,40,30,20,15,10,5,0),
               c(0.13,35,25,15,10,5,0,5),
               c(0.16,30,20,10,5,5,5,15),
               c(0.18,25,15,15,20,25,30,40),
               c(0.20,20,15,15,20,25,30,40),
               c(0.22,15,20,25,30,35,40,50)))
}

#' Surface organic reaction (pH) data table
#'
#' The surface organic reaction (pH) data table
#' @return Surface reaction (pH) data table
#' @export
surfaceOrganicReactionDF <- function(){
  # First column is the soil pH
  return(cbind(c(0,7.5,7,6.5,6,5.5,5,4.5,4,3.5,3),
               c(0.04,50,45,40,40,40,45,50,55,60,70),
               c(0.10,35,30,30,30,30,35,40,45,50,60),
               c(0.16,25,20,20,20,20,25,30,35,45,55),
               c(0.20,15,10,10,10,10,15,20,30,40,50)))
}

#' Surface organic salinity data table
#'
#' The surface organic salinity (dS/m) data table
#' @return Surface salinity (dS/m) data table
#' @export
surfaceOrganicSalinityDF <- function(){
  # First column is the soil salinity
  return(cbind(c(2,4,8,16),
               c(0,20,50,75)))
}
