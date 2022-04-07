# SRS Main

# This library provides the main function body for the Agriculture and
# Agri-Foods Canada's suitability rating system.

# Creation date: Mar 06, 2022
# Last updated: Mar 14, 2022

#' SRS Main Function
#'
#' This function calls the data preparation tools and climate, mineral soil,
#' organic soil, and landscape indices.
#' @param
#' @return
#' @export
#'
#' Corrections:
#' Correct landscape file to be the same as organic and mineral
#' with the "count".

srsMain <- function(cropType,cropArrays,rasterStackFolder,shapefileAOI){

  # '~/Library/Mobile Documents/com~apple~CloudDocs/Desktop - iCloud/AAFC/SRS_V6_3/SRS.6.3.0/data/default_data/test_data_ab/'

  # 1. Data prep tools

  # 1b. Create data tables for each index

  # Prepare climate data for the climate index
  # dataPrep("climate",c("ppe_pei.tif","ppe_Apr_pei.tif","ppe_Sep_pei.tif","apr_sep_egdd_T5_2001-2020_pei.tif"),
  #          FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # Prepare mineral soil data for the mineral soil index
  # dataPrep("mineral",c("ppe_pei.tif","siltcontent_pei.tif","claycontent_pei.tif","organiccarbon_pei.tif","pH_pei.tif","bulkdensity_pei.tif"),
  #          FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # # Prepare organic soil data for the organic soil index
  # dataPrep("organic",c("apr_sep_egdd_T5_2001-2020_pei.tif","ppe_pei.tif","bulkdensity_pei.tif","pH_pei.tif"),
  #          FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # # Prepare landscape data for the landscape index
  # dataPrep("landscape",c("DEM_pei.tif"),
  #          FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))

  # Prepare data for each index
  climateList <- list()
  mineralList <- list()
  organicList <- list()
  landscapeList <- list()

  inputDataList <- list.files(rasterStackFolder)
  if(str_contains(cropType,c("alfalfa","canola","sssg"),logic = "or")){
    cropName <- cropType
    cropType <- "EGDD"
    inputDataList <- purrr::discard(inputDataList,.p = ~stringr::str_detect(.x,"egdd"))
  } else if(str_contains(cropType,c("corn","potato","soybean"),logic = "or")){
    cropName <- cropType
    cropType <- "CHU"
    inputDataList <- purrr::discard(inputDataList,.p = ~stringr::str_detect(.x,"chu"))
  }

  for(i in 1:length(inputDataList)){
    if(str_contains(inputDataList[i],c("ppe_Apr","ppe_Sep","ppe_May","egdd","chu"),logic = "or")){
      climateList <- append(climateList,inputDataList[i])}
    if(str_contains(inputDataList[i],c("ppe_May","silt","clay","organiccarbon","pH","bulk"),logic = "or")){
      mineralList <- append(mineralList,inputDataList[i])}
    if(str_contains(inputDataList[i],c("egdd","chu","ppe_May","pH","bulk"),logic = "or")){
      organicList <- append(organicList,inputDataList[i])}
    if(str_contains(inputDataList[i],c("DEM"),logic = "or")){
      landscapeList <- append(landscapeList,inputDataList[i])}
  }

  dataPrep("climate",climateList,
           rasterStackFolder,shapefileAOI)
  # Prepare mineral soil data for the mineral soil index
  dataPrep("mineral",mineralList,
           rasterStackFolder,shapefileAOI)
  # Prepare organic soil data for the organic soil index
  dataPrep("organic",organicList,
           rasterStackFolder,shapefileAOI)
  # Prepare landscape data for the landscape index
  dataPrep("landscape",landscapeList,
           rasterStackFolder,shapefileAOI)


  # 2. Indices
  # 2a. Climate index
  print("Starting climate index calculation...")

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],"climate") && str_contains(totalFilestemp[i],".tif")){
      totalFiles <- totalFiles + 1
    }
  }


  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/climate_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/climate_table_temp_',i,'.tif')))
    baseClimateRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], c("egdd","chu"),logic = "or")){
          temp <- raster(tempDF, layer = count)
          assign(paste0("climateDF_1"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[j], "ppe_Apr")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_2"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "ppe_Sep")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("climateDF_3"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[j], "ppe")){
          temp <- raster(tempDF, layer = count)
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
    values(baseClimateRaster) <- climateResults
    writePermData(baseClimateRaster,FFP(paste0('/data/temp/results/')),
                  paste0('climateResults_',i),"GTiff")
  }

  # 2b. Mineral soil index
  print("Starting mineral soil index calculation...")

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],"mineral") && str_contains(totalFilestemp[i],".tif")){
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
        if(str_contains(tempOrder[j], "ppe")){
          temp <- raster(tempDF, layer = count)
          assign(paste0("mineralDF_1"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[j], "silt")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_2"),temp)
          temp <- raster(tempDF, layer = (j+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_3"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "clay")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_4"),temp)
          temp <- raster(tempDF, layer = (j+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_5"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "organiccarbon")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_6"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "pH")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_7"),temp)
          temp <- raster(tempDF, layer = (count+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("mineralDF_8"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "bulk")){
          temp <- raster(tempDF, layer = (count+1))
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
    values(baseMineralRaster) <- mineralResults
    writePermData(baseMineralRaster,FFP(paste0('/data/temp/results/')),
                  paste0('mineralResults_',i),"GTiff")
  }

  # 2c. Organic soil index
  print("Starting organic soil calculation...")

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],"organic") && str_contains(totalFilestemp[i],".tif")){
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
          assign(paste0("organicDF_1"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[j], "ppe")){
          temp <- raster(tempDF, layer = count)
          assign(paste0("organicDF_2"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[j], "bulk")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_3"),temp)
          temp <- raster(tempDF, layer = (count+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_4"),temp)
          count <- count + 3
        } else if(str_contains(tempOrder[j], "pH")){
          temp <- raster(tempDF, layer = count)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_5"),temp)
          temp <- raster(tempDF, layer = (count+1))
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
    values(baseOrganicRaster) <- organicResults
    writePermData(baseOrganicRaster,FFP(paste0('/data/temp/results/')),
                  paste0('organicResults_',i),"GTiff")
  }

  # 2d. Landscape index
  print("Starting landscape index calculation...")

  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],"landscape") && str_contains(totalFilestemp[i],".tif")){
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
          assign(paste0("landscapeDF_1"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[j], "slopeLength") || str_contains(tempOrder[j], "lFactor")){
          temp <- raster(tempDF, layer = count)
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
    values(baseLandscapeRaster) <- landscapeResults
    writePermData(baseLandscapeRaster,FFP(paste0('/data/temp/results/')),
                  paste0('landscapeResults_',i),"GTiff")
  }
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

  for(i in 1:(length(totalFiles)/4)){
    baseRaster <- raster(loadRaster(FFP(paste0("/data/temp/results/",totalFiles[i]))))
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

    # Check to see which files exist and calculate rating for each cell if they do
    if(exists("temp1")){
      temp1 <- sapply(get('temp1'),ratingTable)
      tempLength <- length(temp1)
    } else {
      temp1 <- NA
    }
    if(exists("temp2")){
      temp2 <- sapply(get('temp2'),ratingTable)
      tempLength <- length(temp2)
    } else {
      temp2 <- NA
    }
    if(exists("temp3")){
      temp3 <- sapply(get('temp3'),ratingTable)
      tempLength <- length(temp3)
    } else {
      temp3 <- NA
    }
    if(exists("temp4")){
      temp4 <- sapply(get('temp4'),ratingTable)
      tempLength <- length(temp4)
    } else {
      temp4 <- NA
    }

    if(is.na(temp1)){
      temp1 <- rep(0, tempLength)
    }
    if(is.na(temp2)){
      temp2 <- rep(0, tempLength)
    }
    if(is.na(temp3)){
      temp3 <- rep(0, tempLength)
    }
    if(is.na(temp4)){
      temp4 <- rep(0, tempLength)
    }

    tempResults <- mapply(maxFunction,temp1,
                          temp2,
                          temp3,
                          temp4)
    values(baseRaster) <- tempResults
    writePermData(baseRaster,FFP(paste0("/data/temp/results/")),paste0("FinalResults_",i,"_",cropName,".tif"),"GTiff")
  }

}
