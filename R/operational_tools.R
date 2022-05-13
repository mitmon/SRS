# Operational Tools

# This library provides the operational tools for the suitability rating system.

# Creation date: Feb 18, 2022
# Last updated: Mar 13, 2022

####################### Data Management Tools #######################

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

#' Delete folder
#'
#' Delete the folder in the file path
#' @param inputFolder Input folder that will be deleted
#' @export
deleteFolder <- function(inputFile){
  unlink(FFP(inputFile), recursive = TRUE)
}



#' Load Raster
#'
#' Load raster into work space
#' @param inputRaster Location of input raster file
#' @return Raster
#' @export
loadRaster <-function(inputRaster){
  return(stack(inputRaster))
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

#' Write permanent data
#'
#' Write permanent data. This tool will write any data that is currently in
#' the same folder as the input data.
#' @param inputData Input data that needs to be saved
#' @return Saves the input data to a folder location
#' @export
writePermData <- function(inputData, exportLocation, exportName, exportType){

  if(!file.exists(exportLocation)){
    dir.create(exportLocation)
  }

  if(class(inputData)[1] == "RasterLayer" || class(inputData)[1] == "RasterStack" || class(inputData)[1] == "RasterBrick"){
    writeRaster(inputData,
                paste0(exportLocation,exportName),
                bandorder ='BIL',
                format = exportType,
                overwrite = TRUE)
  }

  else if(class(inputData)[1] == "SpatialPolygonsDataFrame"){
    shapefile(inputData, paste0(exportLocation,exportName))
  }
  else {
    stop("Error in writePermData Input data is not a raster stack, raster
               layer, raster brick, or shapefile.")}
}

#' Write temp data
#'
#' Write temporary data to temp folder for further processing
#' @param inputData Input temp data that needs to be save temporarily
#' @param tempLocation Location where temp data will be saved
#' @param tempName Temp name for the files
#' @return Saves the input data to a temporary location
#' @export
writeTempData <- function(inputData, tempFolder, tempName, exportType){
  tempLocation <- FFP(paste0("/data/temp/",tempFolder,"/"))

  if(!file.exists(tempLocation)){
    dir.create(tempLocation)
  }

  if(class(inputData)[1] == "RasterLayer" || class(inputData)[1] == "RasterStack" || class(inputData)[1] == "RasterBrick"){
    writeRaster(inputData,
                paste0(tempLocation,tempName),
                bandorder ='BIL',
                format = exportType,
                overwrite = TRUE)
  }
  else if(class(inputData)[1] == "SpatialPolygonsDataFrame"){
    shapefile(inputData, paste0(tempLocation,tempName))
  } else if(class(inputData[[1]]) == "RasterLayer" || class(inputData[[1]]) == "RasterStack" || class(inputData[[1]]) == "RasterBrick"){
    writeRaster(inputData[[1]],
                paste0(tempLocation,tempName),
                bandorder ='BIL',
                format = exportType,
                overwrite = TRUE)
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

#' Soil texture
#'
#' Determine soil texture based on percentage silt and clay percentages. Future
#' updates will reference soil lookup tables to best determine soil texture and
#' type.
#' @param siltPercent Percentage of silt in soil composition
#' @param clayPercent Percentage of clay in soil composition
#' @return The texture of the soil profile based on percent silt clay
#' @export
soilTexture <- function(siltPercent,clayPercent){
  return((siltPercent + clayPercent))
}

#' Surface and subsurface average calculation
#'
#' Calculate the average surface value.
#' @param divideDepth Depth that delineates the difference between surface and
#' subsurface parameters. The depth is currently default to 60cm but the user can
#' specify an alternative depth. Example, any layer less than 60cm deep is
#' considered surface and any layer greater than 60cm is subsurface.
#' @param inputRaster The input raster that will be used to determine the average
#' surface and subsurface values.
#' @return The average for the surface and subsurface along with average overall
#' @export
surfaceAndSubsurface <- function(divideDepth,inputRaster){

  tempname <- names(inputRaster)
  tempname <- str_split(tempname,"b")
  baseRaster <- raster(inputRaster[[1]])

  for(i in 3:length(tempname)){
    if(!is.na(as.numeric(tempname[[i]][2]))){
      if(as.numeric(tempname[[i]][2]) > divideDepth){
        divide <- i + 1
        break
      }
    } else {
      if(as.numeric(tempname[[i]][3]) > divideDepth){
        divide <- i + 1
        break
      }
    }
  }

  inputRaster <- cbind(xyFromCell(inputRaster, 1:ncell(inputRaster)), values(inputRaster))
  inputRaster <- as.data.frame(inputRaster, xy = TRUE, na.rm = FALSE,optional=TRUE)

  inputRaster$surface <- rowMeans(inputRaster[,c(3:divide)])
  inputRaster$subsurface <- rowMeans(inputRaster[,c(divide:(ncol(inputRaster)-2))])
  inputRaster$all <- rowMeans(inputRaster[,c(3:ncol(inputRaster))])

  tempstack <- stack()

  for(i in (ncol(inputRaster)-2):ncol(inputRaster)){
    values(baseRaster) <- inputRaster[,i]
    tempstack <- stack(tempstack,baseRaster)
  }

  return(tempstack)

}

####################### Spatial Tools #######################

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
#' Batch mask an array of input files.
#' ** This tool is meant to be used to mask dead space around two images. This
#' tool helps with issues of miss-aligned input files by filling the extra space
#' in the larger of the two input files with with NA data.**
#' @param requiredDataArray
#' @param inputFolder Input file array.
#' @param exportFolder Export folder. Location where data is saved.
#' @return Masked input file
#' @export
batchMaskRaster <- function(requiredDataArray, inputFolder, exportFolder){

  # Load files in folder
  listFiles <- list.files(inputFolder)
  # Get only required .tif files
  listFiles <- listFiles[listFiles %in% requiredDataArray]

  # Get extents of the files
  listFiles_data <- lapply(listFiles,
                           function(x)
                             if(str_contains(x,".tif")){
                               loadRaster(paste0(inputFolder,x))
                             } else if(str_contains(x,".shp")){
                               loadShapefile(paste0(inputFolder,x))
                             } else {
                               pass()
                             })

  # Get largest extent
  listFiles_extents <- lapply(listFiles_data, raster::extent)

  if(length(listFiles_extents) > 1){
    do.call(raster::merge, listFiles_extents)
  }

  # Get extents of the files
  listFiles_data <- lapply(listFiles,
                           function(x)
                             if(str_contains(x,".tif")){
                               loadRaster(paste0(inputFolder,x))
                             } else {
                               pass()
                             })

  listFiles_data <- compact(listFiles_data)

  # Reproject files so they are all the same extent
  for(i in 1:length(listFiles_data)){
    replace(listFiles_data,i,reprojectFile(listFiles_data[[i]],"WGS84"))
  }

  # Create temporary Shape file with the bounding box of the largest file
  xMax <- max(sapply(listFiles_extents, function(x) (xmax(x)))) + 1
  yMax <- max(sapply(listFiles_extents, function(x) (ymax(x)))) + 1
  xMin <- min(sapply(listFiles_extents, function(x) (xmin(x)))) - 1
  yMin <- min(sapply(listFiles_extents, function(x) (ymin(x)))) - 1

  # Create a raster with the coordinate for the largest extent
  listFiles_raster <- raster()
  values(listFiles_raster) <- 0
  extent(listFiles_raster) <- c(xMin,xMax,yMin,yMax)
  projection(listFiles_raster) <- CRS("+proj=longlat +datum=WGS84")

  listFilesNew <- c()

  for(i in 1: length(listFiles)){
    if(str_contains(listFiles[i],".tif")){
      listFilesNew <- c(listFilesNew, listFiles[i])
    } else {
      next
    }
  }

  listFiles <- listFilesNew

  # raster(vals=values(r2),ext=extent(r1),crs=crs(r1),
  #        nrows=dim(r1)[1],ncols=dim(r1)[2])

  # Mask files that are not part of the largest extent
  for(i in 1:length(listFiles_data)){
    # tempmask <- crop(listFiles_data[[i]],extent(listFiles_raster))
    # tempmask <- raster(vals=values(tempmask),
    #                    ext=extent(listFiles_raster),
    #                    crs=crs(listFiles_raster),
    #                    nrows=dim(listFiles_raster)[1],
    #                    ncols=dim(listFiles_raster)[2])
    tempmask <- extend(brick(listFiles_data[[i]]),listFiles_raster)
    tempname <- listFiles[i]
    writePermData(tempmask,paste0(exportFolder),paste0("masked_",tempname),'raster')
  }

  print(paste0("Masked ", length(listFiles_data)," files."))
}

#' Batch Mask Folder (Not in use)
#'
#' Batch mask a folder of files.
#' ** This tool is meant to be used to mask dead space around two images. This
#' tool helps with issues of miss-aligned input files by filling the extra space
#' in the larger of the two input files with with NA data.**
#' @param inputFolder Input folder
#' @param exportFolder Export folder. Location where data is saved.
#' @return Masked input file
#' @export
batchMaskFolder <- function(inputFolder, exportFolder){

  listFiles <- list.files(inputFolder)
  # Get only .tif and .shp files
  listFiles <- lapply(listFiles,
                      function(x)
                        if(str_contains(x,".tif")){
                          x
                        } else if(str_contains(x,".shp")){
                          x
                        } else {
                          pass()
                        })
  listFiles <- compact(listFiles)
  # Get extents of the files
  listFiles_data <- lapply(listFiles,
                           function(x)
                             if(str_contains(x,".tif")){
                               loadRaster(paste0(inputFolder,x))
                             } else if(str_contains(x,".shp")){
                               loadShapefile(paste0(inputFolder,x))
                             } else {
                               pass()
                             })
  # Get largest extent
  listFiles_extents <- lapply(listFiles_data, raster::extent)
  do.call(raster::merge, listFiles_extents)

  # Get extents of the files
  listFiles_data <- lapply(listFiles,
                           function(x)
                             if(str_contains(x,".tif")){
                               loadRaster(paste0(inputFolder,x))
                             } else {
                               pass()
                             })
  listFiles_data <- compact(listFiles_data)

  # Reproject files so they are all the same extent
  for(i in 1:length(listFiles_data)){
    replace(listFiles_data,i,reprojectFile(listFiles_data[[i]],"WGS84"))
  }

  # Create temporary Shape file with the bounding box of the largest file
  xMax <- max(sapply(listFiles_extents, function(x) (xmax(x))))
  yMax <- max(sapply(listFiles_extents, function(x) (ymax(x))))
  xMin <- min(sapply(listFiles_extents, function(x) (xmin(x))))
  yMin <- min(sapply(listFiles_extents, function(x) (ymin(x))))

  # Create a raster with the coordinate for the largest extent
  listFiles_raster <- raster()
  values(listFiles_raster) <- 0
  extent(listFiles_raster) <- c(xMin,xMax,yMin,yMax)
  projection(listFiles_raster) <- CRS("+proj=longlat +datum=WGS84")

  # Mask files that are not part of the largest extent
  for(i in 1:length(listFiles_data)){
    tempmask <- extend(listFiles_data[[i]],listFiles_raster)
    writePermData(tempmask,FFP(paste0(exportFolder)),paste0("masked_",names(tempmask)), 'raster')
  }

  print(paste0("Masked ", length(listFiles_data)," files."))
}

#' Batch Crop Raster
#'
#' Batch crop a folder of rasters and save to temporary location
#' @param inputFolder Input raster folder for batch processing
#' @param inputExtent Extent to crop. Input is a shape file or raster
#' @return Cropped input file
#' @export
batchCropRaster <- function(inputFolder,inputExtent){

  # Get input extent type
  inputExtentType <- 0
  if(typeof(inputExtent) == "S4"){
    inputExtentType <- 4
  } else if(str_contains(inputExtent,".shp")){
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

  listFiles <- list.files(inputFolder)

  for(i in 1:length(listFiles)){
    # Determine if the input extent is a shape file, raster layer or
    # coordinates in EPSG4326/WGS84 format.
    if(str_contains(listFiles[i],".tif") || str_contains(listFiles[i],".gri")){

      # Get temp file
      tempFile <- loadRaster(paste0(inputFolder,"/",listFiles[i]))

      if(inputExtentType == 1){
        # Load the shapefile
        inputExtentsf <- loadShapefile(inputExtent)
        lengthinputExtentsf <- length(inputExtentsf)
        # Run function for total number of crops
        for(j in 1:lengthinputExtentsf){
          # Crop tempFile
          tempcrop <- crop(tempFile,inputExtentsf[j,])
          # Write the file to temp location
          writeTempData(tempcrop,paste0("temp_",j,"/"),tempName = (paste0(listFiles[i])))
        }
      }

      else if(inputExtentType == 2) {
        # Load the raster file
        inputExtentr <- loadRaster(inputExtent)
        # Crop tempFile
        tempcrop <- crop(tempFile,inputExtentr)
        # Write the file to temp location
        writeTempData(tempFile,paste0("temp_",j,"/"),tempName = (paste0(listFiles[i])))
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
      else if(inputExtentType == 4){
        # Load the shapefile
        inputExtentsf <- inputExtent
        lengthinputExtentsf <- length(inputExtentsf)
        # Run function for total number of crops
        for(j in 1:lengthinputExtentsf){
          # Crop tempFile
          tempcrop <- crop(tempFile,inputExtentsf[j,])
          # Write the file to temp location
          writeTempData(tempcrop,paste0("temp_",j,"/"),tempName = (paste0(listFiles[i])))
        }
      }

      else {
        stop("Error in batch crop. Input data is not a raster stack, raster
                   layer, or shapefile.")
      }
    } else {
      next
    }
  }

  print(paste0("Cropped ", length(listFiles)," files."))
}

#' Rating table function
#'
#' This tools converts the values from each indices into a rating.
#' @param inputValue The input values to be converted to a rating
#' @return Returns a rating from 1 to 7 with 1 being the best
#' @export
ratingTable <- function(x){
  if(is.na(x)){
    x <- NA
  } else if(x < 10){
    x <- 7
  } else if(x < 20){
    x <- 6
  } else if(x < 30){
    x <- 5
  } else if(x < 45){
    x <- 4
  } else if(x < 60){
    x <- 3
  } else if(x < 80){
    x <- 2
  } else if(x <= 100){
    x <- 1
  }
  return(x)
}

#' Final table function
#'
#' This tools converts the values from each indices into a rating.
#' @param v V is the number of indices that will be used for the results calculation
#' @param w,x,y,z The input climate, mineral soil, organic soil, and landscape
#' calculated values from each index.
#' @return Returns a final rating from 1 to 7 with 1 being the best. Also returns
#' the index values for each respectable index. Climate, mineral soil, organic soil,
#' and landscape are the 2 to n values on each rating
#' @export
maxFunction <- function(w,x,y,z){
  tempMax <- max(w,x,y,z)
  tempMax <- as.integer((tempMax*10000)) + as.integer((w*1000)) + as.integer((x*100)) + as.integer((y*10)) + as.integer((z*1))
  return(tempMax)
}

####################### SRS Specific Tools #######################

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
#' @export
dataPrep <- function(index,requiredDataArray, rasterStackFolder, shapefileAOI){

  # 1. Get required files
  # Determine files to import for the raster stack
  listFiles <- list.files(rasterStackFolder)
  sfname <- c()
  if(typeof(shapefileAOI) != "S4"){
  for(i in 1:length(listFiles)){
    if(str_contains(listFiles[i],".shp")){
      sfname <- listFiles[i]
    } else if (i == length(listFiles) && is_empty(sfname)){
      stop("Error loading shapefile. Please enter a valid .shp file.")
    }
  }}
  # Get only required .tif files
  listFiles <- listFiles[listFiles %in% requiredDataArray]
  # Load the rasters
  listFiles_data <- lapply(listFiles, function(x) loadRaster(paste0(rasterStackFolder,x)))

  # Get number of files in temp folder. This is used to better sort and process
  # recursive stack production.
  if(is_empty(FFP(paste0("/data/temp/")))){
    numTempFiles <- 1
  } else {
    numTempFiles <- length(list.files(FFP(paste0("/data/temp/")))) + 1
  }
  # 2. Align and crop raster files for further processing
  # Mask files so they are all the same extent
  if(typeof(shapefileAOI) != "S4"){
    batchMaskRaster(append(requiredDataArray,sfname[1]),rasterStackFolder,FFP("/data/temp/input_data/"))
  } else {
    batchMaskRaster(requiredDataArray,rasterStackFolder,FFP("/data/temp/input_data/"))
  }

  # Crop files into tiles for quicker processing
  batchCropRaster(FFP(paste0("/data/temp/input_data/")),shapefileAOI)

  # 3. Stack required files based on required data array and export the table
  # Get files in temp folder
  tempListFilesLength <- length(list.files(FFP(paste0("/data/temp/")))) - abs(numTempFiles)

  for(i in 1:(tempListFilesLength)){
    tempListFiles <- list.files(FFP(paste0("/data/temp/temp_",i)))
    j <- 1
    while(j <= length(tempListFiles)){
        if(str_contains(tempListFiles[j], '.gri')){
          j <- j + 1
          next
        }
        # 3a. Determine if surface and subsurface averages are required based on the
        # user request. Call the surface and subsurface function and appending
        # results to the input raster.
        else if(index == "mineral" && str_contains(tempListFiles[j], '.grd')
                  && (str_contains(tempListFiles[j], c('bulk','clay','silt','organic','pH'), logic = 'or'))){
          # Determine if each input file is stacked in one file or separate files
          # If separate files, stack first then run.
          if(str_contains(tempListFiles[j], '.grd')
             && (str_contains(tempListFiles[j], c('b0','b10','b30','b60','b100','b200'), logic = 'or'))){

            # Split the first string to get the file name.
            tempString <- str_split(tempListFiles[j],"[.]")[1]
            # Filter the temp list files to get only the files that are part of the stack
            tempStackListFiles <- str_sort(compact(lapply(tempListFiles, function(x) if(str_contains(x,c(tempString,".grd"), logic = "and")){
                                                                                            return(x)
                                                                                        })),numeric = TRUE)

            tempStackListFiles <- lapply(tempStackListFiles, function(x) return(x))
            for(k in 1:length(tempStackListFiles)){
              if(k == 1){
                # Start stacking the file.
                tempStackFiles <- loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempStackListFiles[[k]])))
              } else {
                tempStackFiles <- stack(tempStackFiles,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempStackListFiles[[k]]))))
              }
            }

            if(j == 1){
              tempRasterStack <- surfaceAndSubsurface(60,tempStackFiles)
            } else {
              tempRasterStack <- stack(tempRasterStack,surfaceAndSubsurface(60,tempStackFiles))
            }
            j <- j + (length(tempStackListFiles)*2) - 1
          } else {
          # If full files, run stack.
            if(j == 1){
              tempRasterStack <- surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
            } else {
              tempRasterStack <- stack(tempRasterStack,surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))))
            }
        }
      }
        else if(index == "organic" && str_contains(tempListFiles[j], '.grd')
                 && (str_contains(tempListFiles[j], c('bulk','pH'), logic = 'or'))){
          # Determine if each input file is stacked in one file or separate files
          # If separate files, stack first then run.
          if(str_contains(tempListFiles[j], '.grd')
             && (str_contains(tempListFiles[j], c('b0','b10','b30','b60','b100','b200'), logic = 'or'))){

            # Split the first string to get the file name.
            tempString <- str_split(tempListFiles[j],"[.]")[1]

            # Filter the temp list files to get only the files that are part of the stack
            tempStackListFiles <- str_sort(compact(lapply(tempListFiles, function(x) if(str_contains(x,c(tempString,".grd"), logic = "and")){
              return(x)
            })),numeric = TRUE)

            tempStackListFiles <- lapply(tempStackListFiles, function(x) return(x))
            for(k in 1:length(tempStackListFiles)){
              if(k == 1){
                # Start stacking the file.
                tempStackFiles <- loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempStackListFiles[[k]])))
              } else {
                tempStackFiles <- stack(tempStackFiles,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempStackListFiles[[k]]))))
              }
            }

            if(j == 1){
              tempRasterStack <- surfaceAndSubsurface(60,tempStackFiles)
            } else {
              tempRasterStack <- stack(tempRasterStack,surfaceAndSubsurface(60,tempStackFiles))
            }

            # # Remove the files from the list to remove duplication issues
            # tempListFiles <- compact(lapply(tempListFiles, function(x) if(!str_contains(x,tempString)){return(x)}))
            j <- j + (length(tempStackListFiles)*2) - 1
          } else {
            # If full files, run stack.
            if(j == 1){
              tempRasterStack <- surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
            } else {
              tempRasterStack <- stack(tempRasterStack,surfaceAndSubsurface(60,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))))
            }
          }
      }
        else if(index == "landscape"){
          if(str_contains(tempListFiles[j], '.grd') && (str_contains(tempListFiles[j], 'slopePercent') ||
                                                        str_contains(tempListFiles[j], 'slopeLength') ||
                                                        str_contains(tempListFiles[j], 'lFactor'))){
            if(j == 1){
              tempRasterStack <- loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))
            } else {
              tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
            }
          } else if (str_contains(tempListFiles[j], '.grd') && (str_contains(tempListFiles[j], 'DEM'))){
            if(j == 1){
              lsFunction(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])),i)
              tempRasterStack <-  loadRaster(FFP(paste0("/data/temp/temp_",i,"/slope_",i)))
              tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/lFactor_",i))))
            } else {
              lsFunction(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])),i)
              tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/slope_",i))))
              tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/lFactor_",i))))
            }
          }

      }
        else {
          if(index == "landscape" && str_contains(tempListFiles[j], '.grd')
                                  && str_contains(tempListFiles[j], '.gri')
                                  && (str_contains(tempListFiles[j], 'DEM'))){
            pass()
          }
          else if(j == 1){
            tempRasterStack <- loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j])))
          } else {
            tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/temp_",i,"/",tempListFiles[j]))))
          }
        }

        # 3b. Save the input files name for later when loading data again. The
        # column names will be used later.
        if(index != "landscape" || (str_contains(tempListFiles[j], 'slopePercent') ||
                                    str_contains(tempListFiles[j], 'slopeLength') ||
                                    str_contains(tempListFiles[j], 'lFactor'))){
          if(!file.exists(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")))){
            fileLocation <- FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt"))
            file.create(fileLocation)
            writeLines(paste0(tempListFiles[[j]]),fileLocation)
          }
          else {
            fileLocation <- file(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")))
            fileDataTemp <- readLines(fileLocation)
            writeLines(paste0(fileDataTemp,",",tempListFiles[j]),fileLocation)
          }
        }
        else {
            if(!file.exists(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")))){
              fileLocation <- FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt"))
              file.create(fileLocation)
              writeLines(paste0("slopePercent",",slopeLength"),fileLocation)
            } else {
              fileLocation <- file(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")))
              fileDataTemp <- readLines(fileLocation)
              writeLines(paste0(fileDataTemp,",","slopePercent",",","slopeLength"),fileLocation)
            }
        }
        j <- j + 1
      }

      # 3c. Write data table to data table processing file and copy processing
      # order text over.
      writePermData(tempRasterStack, FFP(paste0('/data/temp/dataTable/')), paste0(index,'_table_temp_',i),'GTiff')
      if(file.exists(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")))){
        file.copy(FFP(paste0("/data/temp/temp_",i,"/",index,"_processOrder_",i,".txt")),FFP(paste0('/data/temp/dataTable/')))
      }
      # 3d. Remove the temp files.
      unlink(FFP(paste0('/data/temp/temp_',i)), recursive = TRUE)
  }
      unlink(FFP(paste0('/data/temp/input_data')), recursive = TRUE)
}

####################### Dev Tools #######################

#' Batch run SRS main
#'
#' This tools is a development tool used to run the SRS main function with entered
#' crop types.
#' @param cropTypeList The crop type.
#' @param cropArrays The arrays used for each specific crop variable.
#' @param inputFolderLocation Input folder location.
#' @param inputShapefile Input shapefile with folder location.
#' @returns Results of the SRS main.
batchRunSRSMain <- function(cropTypeList, cropArrays, inputFolderLocation, inputShapefile){
  for(i in 1:length(cropTypeList)){
    print(paste0("Starting ", i, " of ", length(cropTypeList),"!"))
    srsMain(cropTypeList[i],cropArrays[[i]],inputFolderLocation,inputShapefile)
    print(paste0("Finished ", i, " of ", length(cropTypeList),"!"))
  }
  print("Finished SRS main function.")
}
