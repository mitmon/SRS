# Data preparation tools

# This library provides the data preparation tools for the Agriculture and
# Agri-Foods Canada's suitability rating system.

# Creation date: Feb 24, 2022
# Last updated: Feb 24, 2022

#' Data preparation tool
#'
#' This tools prepares input raster data for further processing. The result is
#' a raster stack that will be later used to calculate the rating tables
#' for climate, mineral, organic, and landscape indices.
#' @param index The index is the type of index the user is requesting. Options
#' are climate, soil, or landscape. Future versions will expand the indices.
#' @param requiredDataArray
#' @param rasterStackFolder
#' @param shapefileAOI Area of interest for the SRS calculation. Shapefile format.
dataPrep <- function(index,requiredDataArray, rasterStackFolder, shapefileAOI){

  # 1. Get required files
  # Determine files to import for the raster stack
  listFiles <- list.files(rasterStackFolder)
  sfname <- c()
  for(i in 1:length(listFiles)){
    if(str_contains(listFiles[i],".shp")){
      sfname <- listFiles[i]
    }
    else if (i == length(listFiles) && is_empty(sfname)){
      stop("Error loading shapefile. Please enter a valid .shp file.")
    }
  }
  # Get only required .tif files
  listFiles <- listFiles[listFiles %in% requiredDataArray]
  # Load the rasters
  listFiles_data <- lapply(listFiles, function(x) loadRaster(FFP(paste0("/data/default_data/test_data/",x))))

  # 2. Align and crop raster files for further processing
  # Mask files so they are all the same extent
  batchMaskRaster(append(requiredDataArray,sfname[1]),rasterStackFolder,FFP("/data/temp/input_data/"))
  # Crop files into tiles for quicker processing
  batchCropRaster(FFP(paste0("/data/temp/input_data/")),shapefileAOI)

  # 3. Stack required files based on required data array and export the table
  # Get files in temp folder
  for(i in 1:(length(list.files(FFP(paste0("/data/temp/"))))-1)){
    tempListFiles <- list.files(FFP(paste0("/data/temp/temp_",i)))

    for(j in 1:(length(tempListFiles))){

      # 3a. Determine if surface and subsurface averages are required based on the
      # user request. Call the surface and subsurface function and appending
      # results to the input raster.
      if(index == "mineral"){
        if(j == 1){
          tempRasterStack <- surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
      } else {
        tempRasterStack <- stack(tempRasterStack,surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))))
        }
      } else {
      if(j == 1){
        tempRasterStack <- loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))
      } else {
        tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
      }
    }

    # 3b. Save the input files name for later when loading data again. The
    # column names will be used later.
    if(!file.exists(FFP(paste0("/data/temp/temp_",i,"/processOrder.txt")))){
      fileLocation <- FFP(paste0("/data/temp/temp_",i,"/processOrder.txt"))
      file.create(fileLocation)
      writeLines(tempListFiles[[j]],fileLocation)
    } else {
      fileLocation <- file(FFP(paste0("/data/temp/temp_",i,"/processOrder.txt")))
      fileDataTemp <- readLines(fileLocation)
      writeLines(paste0(fileDataTemp,",",tempListFiles[[j]]),fileLocation)
    }
      }
    # Save temporary raster table file
    if(index == "landscape"){
      writePermData(tempRasterStack, FFP(paste0('/data/temp/temp_',i,'/')), paste0('DEM'),'GTiff')
    } else {
    writePermData(tempRasterStack, FFP(paste0('/data/temp/temp_',i,'/')), paste0('table_temp_',i),'GTiff')
    }
  }
}
