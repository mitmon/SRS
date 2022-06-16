# SRS Main

# This library provides the main function body for the Agriculture and
# Agri-Foods Canada's suitability rating system.

# Creation date: Mar 06, 2022
# Last updated: Mar 14, 2022

#' SRS Main Function
#'
#' This function calls the data preparation tools and climate, mineral soil,
#' organic soil, and landscape indices.
#' @param cropType The crop type that will be used for the calculations
#' @param cropArrays The input default data or custom data for each crop type
#' @param rasterStackFolder The input folder location containing the required data
#' @param shapefileAOI The input shapefile to be used for the area of interest
#' @param indicesCalc A list of the input indices that will be calculated
#' (options are climate, mineral, organic, and landscape)
#' @param saveLocation The folder location where the data will be saved
#' @return The return is a grid data (.tiff) file containing the resulting calculations of SRS
#' @export
srsMain <- function(cropType,cropArrays,rasterStackFolder,shapefileAOI,indicesCalc,saveLocation){

  # setwd("../SRS/")

  # 1. Data prep tools
  print("Starting data prep tools...")

  # 1a. Clear temp for future processing
  if(!dir.exists(paste0(tempdir(),"/data/temp/"))){
    dir.create(paste0(tempdir(),"/data/"))
    dir.create(paste0(tempdir(),"/data/temp/"))
  } else {
    deleteFolder(paste0(tempdir(),"/data/"))
    dir.create(paste0(tempdir(),"/data/"))
    dir.create(paste0(tempdir(),"/data/temp/"))
  }

  # 1b. Create data tables for each index
  # Prepare data for each index
  climateList <- list()
  mineralList <- list()
  organicList <- list()
  landscapeList <- list()

  inputDataList <- list.files(rasterStackFolder)

  if(str_contains(cropType,c("alfalfa","canola","sssg","peas","lentils","dry beans","faba beans","chickpeas"),logic = "or")){
    cropName <- cropType
    cropType <- "EGDD"
    inputDataList <- purrr::discard(inputDataList,.p = ~str_detect(.x,"chu"))
  } else if(str_contains(cropType,c("corn","potato","soybean"),logic = "or")){
    cropName <- cropType
    cropType <- "CHU"
    inputDataList <- discard(inputDataList,.p = ~str_detect(.x,"egdd"))
  }

  for(i in 1:length(inputDataList)){
    if(cropType == "EGDD"){
      if(str_contains(inputDataList[i],c("ppe_spr","ppe_fall","ppe_grow","ppeSpr","ppeFall","ppeGrow","egdd"),logic = "or")){
        climateList <- append(climateList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("ppe_grow","ppeGrow","silt","clay","organiccarbon","pH","bulk"),logic = "or")){
        mineralList <- append(mineralList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("egdd","ppe_grow","ppeGrow","pH","bulk"),logic = "or")){
        organicList <- append(organicList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("DEM","elevation","slopePercent","slopeLength","lFactor"),logic = "or")){
        landscapeList <- append(landscapeList,inputDataList[i])}
    } else if(cropType == "CHU"){
      if(str_contains(inputDataList[i],c("ppe_spr","ppe_fall","ppe_grow","ppeSpr","ppeFall","ppeGrow","chu"),logic = "or")){
        climateList <- append(climateList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("ppe_grow","ppeGrow","silt","clay","organiccarbon","pH","bulk"),logic = "or")){
        mineralList <- append(mineralList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("chu","ppe_grow","ppeGrow","pH","bulk"),logic = "or")){
        organicList <- append(organicList,inputDataList[i])}
      if(str_contains(inputDataList[i],c("DEM","elevation","slopePercent","slopeLength","lFactor"),logic = "or")){
        landscapeList <- append(landscapeList,inputDataList[i])}
    }
  }

  # Prepare climate data for the climate index
  if("climate" %in% indicesCalc){
    # Update user. Show notification.
    showNotification("Starting climate data preprocessing...")
    # Call the data prep function.
    dataPrep("climate",climateList,
           rasterStackFolder,shapefileAOI)
    # Update user. Show notification.
    showNotification("Completed climate data preprocessing")
    # Update progress bar
    updateProgressBar(
        id = "pb1",
        value = 5
      )
  }

  # Prepare mineral soil data for the mineral soil index
  if("mineral" %in% indicesCalc){
    # Update user. Show notification.
    showNotification("Starting mineral soil data preprocessing...")
    # Call the data prep function.
    dataPrep("mineral",mineralList,
           rasterStackFolder,shapefileAOI)
    # Update user. Show notification.
    showNotification("Completed mineral soil data preprocessing")
    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 10
    )
  }
  # Prepare organic soil data for the organic soil index
  if("organic" %in% indicesCalc){
    # Update user. Show notification.
    showNotification("Starting organic soil data preprocessing...")
    # Call the data prep function.
    dataPrep("organic",organicList,
             rasterStackFolder,shapefileAOI)
    # Update user. Show notification.
    showNotification("Completed organic soil data preprocessing")
    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 15
    )
  }
  # Prepare landscape data for the landscape index
  if("landscape" %in% indicesCalc){
    # Update user. Show notification.
    showNotification("Starting landscape data preprocessing...")

    # Call the data prep function.
    # If the user enters DEM files instead of LS use this. Else.
    if(!str_contains(landscapeList,c("slopePercent","slopeLength","lFactor"),logic = "or")){
      dataPrep("landscape",landscapeList,
               rasterStackFolder,shapefileAOI)
    } else {
      dataPrep("landscape",landscapeList,
               rasterStackFolder, shapefileAOI)
    }

    # Update user. Show notification.
    showNotification("Completed landscape data preprocessing")
    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 30
    )
  }

  # 2. Indices
  # 2a. Climate index
  if("climate" %in% indicesCalc){
    showNotification("Starting climate index calculation...")
    # print("Starting climate index calculation...")

    updateProgressBar(
      id = "pb1",
      value = 35
    )

    # Obtain the total number of temp folder that were made.
    # Use this number for the number of times the system has to process the data
    totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
    totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],c("climate",".tif"),logic = "and") &&
       !str_contains(totalFilestemp[i],c("aux","xml"),logic = "or")){
      totalFiles <- totalFiles + 1
    }
  }
  # Split the layers of the input data to the appropriate variable
  for(i in 1:totalFiles){
    # Load the process order file and count number of data files used to create input data.
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/climate_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    # Load the input raster
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/climate_table_temp_',i,'.tif')))
    # Set the input raster as the base file for which more data will be written to.
    baseClimateRaster <- raster(tempDF)
    # Body of climate processing function
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], c("egdd","chu"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_1"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], c("ppe_spr","ppeSpr"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_2"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], c("ppe_fall","ppeFall"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_3"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j],  c("ppe_grow","ppeGrow"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_4"),temp)
          count <- count + 1
        }
      } else if(j < length(tempDF[1])){
        next
      } else {
        break
      }
      # In future versions, allow for adjustable number of input parameters.
    }

    cropArrays1 <- list(list(cropArrays[1,]))
    cropArrays2 <- list(list(cropArrays[2,]))
    cropArrays3 <- list(list(cropArrays[3,]))
    cropArrays4 <- list(list(cropArrays[4,]))

    climateResults <- matrix(mapply(climateIndexMain,
                                    cropArrays1,
                                    cropArrays2,
                                    cropArrays3,
                                    cropArrays4,
                                    get(paste0('climateDF_4'))[3],
                                    get(paste0('climateDF_1'))[3],
                                    get(paste0('climateDF_2'))[3],
                                    get(paste0('climateDF_3'))[3],
                                    cropType),ncol = 1)

    baseClimateRaster <- setValues(baseClimateRaster,climateResults)
    # Legacy code
    # values(baseClimateRaster) <- climateResults

    # writePermData(baseClimateRaster,('/Users/mitmon/Library/Mobile Documents/com~apple~CloudDocs/Desktop - iCloud/AAFC/SRS_V6_3/SRS_V6_Shiny_App/'),
    #               paste0('climateInter_',i),"GTiff")

    writePermData(baseClimateRaster,FFP(paste0('/data/temp/results/')),
                  paste0('climateResults_',i),"GTiff")

    showNotification("Completed climate index calculation")

  }}

  # 2b. Mineral soil index
  if("mineral" %in% indicesCalc){
    showNotification("Starting mineral soil index calculation...")
  # print("Starting mineral soil index calculation...")

    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 50
    )

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],c("mineral",".tif"),logic = "and") &&
       !str_contains(totalFilestemp[i],c("aux","xml"),logic = "or")){
      totalFiles <- totalFiles + 1
    }
  }

  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/mineral_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/mineral_table_temp_',i,'.tif')))
    baseMineralRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], c("ppe_grow","ppeGrow"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_1"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "silt")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_2"),temp)
          temp <- raster(tempDF, layer = (j+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_3"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "clay")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_4"),temp)
          temp <- raster(tempDF, layer = (j+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_5"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "organiccarbon")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_6"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "pH")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_7"),temp)
          temp <- raster(tempDF, layer = (count+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_8"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "bulk")){
          temp <- raster(tempDF, layer = (count+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_9"),temp)
          count <- count + 3
        }
      } else if(j < length(tempDF[1])){
        next
      } else {
        break
      }
      # In future versions, allow for adjustable number of input parameters.
    }

    mineralResults <- matrix(mapply(mineralSoilIndexMain,
                                    get(paste0('mineralDF_1'))[3],
                                    get(paste0('mineralDF_2'))[3],
                                    get(paste0('mineralDF_4'))[3],
                                    get(paste0('mineralDF_3'))[3],
                                    get(paste0('mineralDF_5'))[3],
                                    125,
                                    get(paste0('mineralDF_6'))[3],
                                    NA,
                                    get(paste0('mineralDF_7'))[3],
                                    NA,
                                    NA,
                                    NA,
                                    get(paste0('mineralDF_9'))[3],
                                    NA,
                                    get(paste0('mineralDF_8'))[3],
                                    NA,
                                    NA),ncol = 1)

    baseMineralRaster <- setValues(baseMineralRaster,mineralResults)
    # Legacy code
    # values(baseMineralRaster) <- mineralResults
    writePermData(baseMineralRaster,FFP(paste0('/data/temp/results/')),
                  paste0('mineralResults_',i),"GTiff")

    showNotification("Completed mineral soil index calculation")

  }}

  # 2c. Organic soil index
  if("organic" %in% indicesCalc){
    showNotification("Starting organic soil index calculation...")
  # print("Starting organic soil calculation...")

    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 65
    )

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],c("organic",".tif"),logic = "and") &&
       !str_contains(totalFilestemp[i],c("aux","xml"),logic = "or")){
      totalFiles <- totalFiles + 1
    }
  }

  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/organic_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/organic_table_temp_',i,'.tif')))
    baseOrganicRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], c("egdd","chu"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_1"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "ppe")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_2"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "bulk")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_3"),temp)
          temp <- raster(tempDF, layer = (count+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_4"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "pH")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_5"),temp)
          temp <- raster(tempDF, layer = (count+1))
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_6"),temp)
          count <- count + 3
        }
      } else if(j < length(tempDF[1])){
        next
      } else {
        break
      }
      # In future versions, allow for adjustable number of input parameters.
    }
    organicResults <- matrix(mapply(organicSoilIndexMain,
                                    get(paste0('organicDF_1'))[3],
                                    get(paste0('organicDF_2'))[3],
                                    get(paste0('organicDF_3'))[3],
                                    get(paste0('organicDF_4'))[3],
                                    125,
                                    get(paste0('organicDF_5'))[3],
                                    NA,
                                    get(paste0('organicDF_6'))[3],
                                    NA),ncol = 1)
    baseOrganicRaster <- setValues(baseOrganicRaster,organicResults)
    # Legacy code
    # values(baseOrganicRaster) <- organicResults
    writePermData(baseOrganicRaster,FFP(paste0('/data/temp/results/')),
                  paste0('organicResults_',i),"GTiff")

    showNotification("Completed organic soil index calculation")

  }}

  # 2d. Landscape index
  if("landscape" %in% indicesCalc){
    showNotification("Starting landscape index calculation...")
  # print("Starting landscape index calculation...")

    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 80
    )

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],c("landscape",".tif"),logic = "and") &&
       !str_contains(totalFilestemp[i],c("aux","xml"),logic = "or")){
      totalFiles <- totalFiles + 1
    }
  }

  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/landscape_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/landscape_table_temp_',i,'.tif')))
    baseLandscapeRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], "slopePercent")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("landscapeDF_1"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "slopeLength") || str_contains(tempOrder[j], "lFactor")){
          temp <- raster(tempDF, layer = count)
          # temp <- rasterToPoints(temp, xy = TRUE)
          temp <- cbind(xyFromCell(temp, 1:ncell(temp)), getValues(temp))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("landscapeDF_2"),temp)
          count <- count + 1
        }
      } else if(j < length(tempDF[1])){
        next
      } else {
        break
      }
      # In future versions, allow for adjustable number of input parameters.
    }
    landscapeResults <- matrix(mapply(landscapeIndexMain,
                                      get(paste0('landscapeDF_1'))[3],
                                      get(paste0('landscapeDF_2'))[3],
                                      NA,
                                      NA,
                                      NA),ncol = 1)
    baseLandscapeRaster <- setValues(baseLandscapeRaster,landscapeResults)
    # values(baseLandscapeRaster) <- landscapeResults
    writePermData(baseLandscapeRaster,FFP(paste0('/data/temp/results/')),
                  paste0('landscapeResults_',i),"GTiff")

    showNotification("Completed landscape index calculation")
  }}
  # 3. Final rating

  # Check to make sure folder exists.
  if(!file.exists(FFP(paste0("/data/temp/results/")))){
    fileLocation <- FFP(paste0("/data/temp/results/"))
    file.create(fileLocation)
  }

  # Stack all the data layers and select the worst result from all the indices
  totalFiles <- list.files(FFP(paste0("/data/temp/results/")))
  baseRaster <- NULL
  tempResults <- NULL

  if(typeof(shapefileAOI) == "S4"){
    Indices <- length(shapefileAOI)
  } else {
    Indices <- length(loadShapefile(shapefileAOI))
  }

  for(i in 1:Indices){
    baseRaster <- raster(loadRaster(FFP(paste0("/data/temp/results/",totalFiles[i]))))

    temp1 <- NULL
    temp2 <- NULL
    temp3 <- NULL
    temp4 <- NULL

    for(j in 1:length(totalFiles)){
      if(str_contains(paste0("climateResults_",i,".tif"),totalFiles[j])){
        tempRaster <- loadRaster(FFP(paste0("/data/temp/results/",totalFiles[j])))
        assign("temp1",tempRaster)
      } else if(str_contains(paste0("mineralResults_",i,".tif"),totalFiles[j])){
        tempRaster <- loadRaster(FFP(paste0("/data/temp/results/",totalFiles[j])))
        assign("temp2",tempRaster)
      } else if (str_contains(paste0("organicResults_",i,".tif"),totalFiles[j])){
        tempRaster <- loadRaster(FFP(paste0("/data/temp/results/",totalFiles[j])))
        assign("temp3",tempRaster)
      } else if(str_contains(paste0("landscapeResults_",i,".tif"),totalFiles[j])){
        tempRaster <- loadRaster(FFP(paste0("/data/temp/results/",totalFiles[j])))
        assign("temp4",tempRaster)
        # assign("temp4",sapply(raster(tempRaster),ratingTable))
      }
    }

    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 90
    )

    # Check to see which files exist and calculate rating for each cell if they do
    if(exists("temp1") && !is.null(temp1)){
      temp1 <- sapply(get('temp1'),ratingTable)
      tempLength <- length(temp1)
    }
    if(exists("temp2") && !is.null(temp2)){
      temp2 <- sapply(get('temp2'),ratingTable)
      tempLength <- length(temp2)
    }
    if(exists("temp3") && !is.null(temp3)){
      temp3 <- sapply(get('temp3'),ratingTable)
      tempLength <- length(temp3)
    }
    if(exists("temp4") && !is.null(temp4)){
      temp4 <- sapply(get('temp4'),ratingTable)
      tempLength <- length(temp4)
    }

    if(is.null(temp1)){
      temp1 <- rep(0, tempLength)}
    if(is.null(temp2)){
      temp2 <- rep(0, tempLength)}
    if(is.null(temp3)){
      temp3 <- rep(0, tempLength)}
    if(is.null(temp4)){
      temp4 <- rep(0, tempLength)}

    tempResults <- mapply(maxFunction,temp1,
                          temp2,
                          temp3,
                          temp4)
    baseRaster <- setValues(baseRaster,tempResults)
    # Legacy code
    # values(baseRaster) <- tempResults

    # Update progress bar
    updateProgressBar(
      id = "pb1",
      value = 95
    )

    writePermData(baseRaster,saveLocation,paste0("FinalResults_",i,"_",cropName,".tif"),"GTiff")
    # writePermData(baseRaster,saveLocation,paste0(saveName,".tif"),"GTiff")

    # return(baseRaster)
  }

  # 4. Clear temp for future processing
  if(dir.exists(FFP(paste0("/data/temp/")))){
    deleteFolder(paste0("/data/temp/"))
  }

  # Update progress bar
  updateProgressBar(
    id = "pb1",
    value = 100
  )
  showNotification(paste0("Completed ", cropName, " calculation"))
  # print(paste0("Completed ", cropName, " calculation."))

}
