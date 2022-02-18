# Operational Tools

# Data Management Tools

#' Pass function
#'
#' Used to pass
#' @export
pass = function(){
}

#' Full path string of file
#'
#' Used to get the full path of a file.
#' @param inputFile Input file that needs to return the full path
#' @return String of full path of the file
#' @export
FFP <- function(inputFile){
  return(paste0(getwd(),inputFile))
}

#' Load Raster
#'
#' Load raster into work space
#' @param raster Location of input raster file
#' @return Raster stack
#' @export
loadRaster <-function(raster){
    return(stack(raster))
}

#' Shapefile (Vector file)
#'
#' Load shapefile into work space
#' @param shapefile Location of input shapefile
#' @return Shapefile
#' @export
loadShapefile <-function(shapefile){
  return(shapefile(shapefile))
}

#' Write temp data
#'
#' Write temporary data to temp folder for further processing
#' @param inputData Input temp data that needs to be save temporarily
#' @param tempLocation Location where temp data will be saved
#' @param tempName Temp name for the files
#' @return Saves the input data to a temporary location
#' @export
writeTempData <- function(inputData, tempName){
  tempLocation <-paste0(getwd(),"/data/temp/")

  if(!file.exists(tempLocation)){
    dir.create(paste0(tempLocation))
  }

  if(class(inputData)[1] == "RasterLayer"){
    writeRaster(inputData,
                paste0(tempLocation,tempName),
                format = "GTiff",
                overwrite = TRUE)
  }
  else if(class(inputData)[1] == "RasterStack"){
    writeRaster(inputData,
                paste0(tempLocation,tempName),
                format = "GTiff",
                overwrite = TRUE)
  }
  else if(class(inputData)[1] == "SpatialPolygonsDataFrame"){
    shapefile(inputData, paste0(tempLocation,tempName))
  }
  else {
    stop("Error in writeTempData. Input data is not a raster stack, raster
               layer, or shapefile.")}
}


#' Erase temp data
#'
#' Erase temporary data
#' @param file
#' @return Confirmation of deletion
#' @export
eraseTempData <- function(file){
  file.remove(paste0(file))
  return(print(paste0("Successfully deleted: ", file)))
}

# Spatial Tools

#' Reproject input data
#'
#' Reproject the input data to WGS 1984, EPSG4326
#' @param inputFile Input data
#' @param projection Projection to reproject to. Default is EPSG4326.
#' @return Reprojected input data file
#' @export
reprojectFile <- function(inputFile,projection){
  projection <- paste0("+proj=longlat +datum=",projection," +no_defs")

  if(class(inputFile)[1] == "RasterLayer"){
    return(projectRaster(inputFile,crs = projection))
  }
  else if(class(inputFile)[1] == "RasterStack") {
    return(projectRaster(raster(inputFile),crs = projection))
  }
  else if(class(inputFile)[1] == "SpatialPolygonsDataFrame") {
    proj4string(inputFile) <- CRS(projection)
    return(inputFile)
  }
  else {
    stop("Error in reprojecting. Input data is not a raster stack, raster
               layer, or shapefile.")}
}

#' Batch Mask Raster
#'
#' Batch mask a folder of files.
#' ** This tool is meant to be used to mask dead space around two images. This
#' tool helps with issues of miss-aligned input files by filling the extra space
#' in the larger of the two input files with with NA data.**
#' @param inputFolder
#' @param inputExtent Extent to mask to. Input is a shape file or raster
#' @return Masked input file
#' @export
batchMaskRaster <- function(inputFolder){

  listFiles <- list.files(inputFolder)
  # Get only .tif and .shp files
  listFiles <- lapply(listFiles,
                      function(x)
                        if(str_contains(x,".tif")){
                          FFP(paste0("/data/default_data/test_data/",x))
                        } else if(str_contains(x,".shp")){
                          FFP(paste0("/data/default_data/test_data/",x))
                        } else {
                          pass()
                        })
  listFiles <- compact(listFiles)
  # Get extents of the files
  listFiles_extents <- lapply(listFiles,
                      function(x)
                        if(str_contains(x,".tif")){
                          loadRaster(FFP(paste0("/data/default_data/test_data/",x)))
                        } else if(str_contains(x,".shp")){
                          loadShapefile(FFP(paste0("/data/default_data/test_data/",x)))
                        } else {
                          pass()
                        })
  # Get largest extent
  listFiles_extents <- lapply(listFiles_extents, raster::extent)
  do.call(raster::merge, listFiles_extents)
  listFiles_extents <- st_as_sf(coords = c(xmin(listFiles_extents[[1]]),xmax(listFiles_extents[[1]]),
                                           ymin(listFiles_extents[[1]]),ymax(listFiles_extents[[1]])))
  # Mask files that are not part of the largest extent
  for(i in 1:length(listFiles)){
    tempFile <- FFP(paste0("/data/default_data/test_data/",listFiles[i]))
    if(extent(tempFile) != listFiles_extents[[1]]){
      return(mask(tempFile, mask = ))
    }

  }
  print(paste0("Masked ", length(listFiles)," files."))
}

#' Batch Crop Raster
#'
#' Batch crop a folder of rasters and save to temporary location
#' @param inputFolder Input raster folder for batch processing
#' @param inputExtent Extent to crop. Input is a shape file or raster
#' @return Cropped input file
#' @export
batchCropRaster <- function(inputFolder,inputExtent){

  listFiles <- list.files(inputFolder)

  for(i in 1:length(listFiles)){
    # Determine if the input extent is a shape file, raster layer or
    # coordinates in EPSG4326/WGS84 format.

    # Get temp file
    tempFile <- loadRaster(paste0(inputFolder,"/",listFiles[i]))
    # Get input extent type
    inputExtentType <- 0
    if(str_contains(inputExtent,".shp")){
      inputExtentType <- 1
    }
    else if(str_contains(inputExtent,".tif")){
      inputExtentType <- 2
    }
    else if(str_contains(inputExtent,"UDAOI")){
      inputExtentType <- 3
    } else {
      inputExtentType <- NA
    }

    if(inputExtentType == 1){
      # Load the shapefile
      inputExtent <- loadShapefile(inputExtent)
      # Run function for total number of crops
      for(j in 1:length(inputExtent$Id)){
        # Crop tempFile
        tempcrop <- crop(tempFile,inputExtent[j,])
        # Write the file to temp location
        writeTempData(tempcrop,tempName = (paste0("temp_",j,"/",listFiles[i])))
      }
    }

    else if(inputExtentType == 2) {
      # Load the raster file
      inputExtent <- loadRaster(inputExtent)
      # Crop tempFile
      tempcrop <- crop(tempFile,inputExtent)
      # Write the file to temp location
        writeTempData(tempFile,tempName = (paste0("temp_",j,"/",listFiles[i])))
    }

    ######## Add this part in the future ########

    # else if(inputExtentType == 3) {
    #   # Load the raster file
    #   inputExtent <- loadRaster(inputExtent)
    #   # Crop tempFile
    #   tempcrop <- crop(tempFile,inputExtent)
    #   # Write the file to temp location
    #   writeTempData(tempFile,tempName = (paste0("temp_",j,"/",listFiles[i])))
    # }
    else {
      stop("Error in batch crop. Input data is not a raster stack, raster
                 layer, or shapefile.")
    }
  }
}

