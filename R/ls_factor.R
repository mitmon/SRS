# LS Factor Calculator

# This library calculates the slope length calculation.

# Creation date: Apr 1, 2022
# Last updated: Apr 1, 2022

#' LS factor
#'
#' The ls factor is the main function that calls the required functions to create
#' an ls layer. This code has been adopted and converted from the original work
#' completed by Amanda Moody, Central Washington University, Ellensburg, WA. 2019.
#' Although adopted from the source, the code has been changed significantly to increase
#' speed, remove redundent code and improve overall performance.
#' @param DEM Input digital elevation model that will be used to calculate the LS factor.
#' @return The ls factor and the slope over the DEM.
#' @export
lsFactor <- function(DEM){

  # 1. Define constants
  rasterlist <- array()
  rasterName <- array()
  baseRaster <- raster(DEM)
  values(baseRaster) <- NA
  nRows <- nrow(DEM)
  nCols <- ncol(DEM)
  cTotal <- nCols*nRows
  cSize <- res(DEM)
  cSize <- cSize[1]
  cSize[cSize < 1] <- 250
  fDirc <- c(1,2,4,8,16,32,64,128)
  rookDirc <- c(1,4,16,64)
  diagDirc <- c(2,8,32,128)
  fdCol = c(1, 1, 0, -1, -1, -1, 0, 1)
  fdRow = c(0, 1, 1, 1, 0, -1, -1, -1)

  # 2. Determine max downhill slope array
  # Calculate flow direction
  flowDir <- terrain(DEM, opt = "flowdir")
  maxDHS <- mapply(maxDHSArray, DEM,flowDir)


}

#' Max downhill slope array
#'
#' The max downhill slope array function
#' @param elevPixel Elevation of the pixel
#' @param flowDir This is the flow direction.
#' @return The max downhill slope for each cell
#' @export
maxDHSArray <- function(elevPixel,flowDir){

  # 1. Determine if there are any errors or missing data in the input data. If so
  # return NA and continue to the next.
  # 1a. Determine if the elevation pixel or flow direction are NA.
  if(is.na(elevPixel) || is.na(flowDir)){
    return(NA)
  }

  # 2. Get the direction of flow for each cell.
  # 1b. Determine of the new cells are outside the DEM coverage



}
