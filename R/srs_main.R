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

  # 1. Data prep tools

  # 1b. Create data tables for each index

  # Prepare climate data for the climate index
  dataPrep("climate",c("ppe_pei.tif","ppe_Apr_pei.tif","ppe_Sep_pei.tif","apr_sep_egdd_T5_2001-2020_pei.tif"),
           FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # Prepare mineral soil data for the mineral soil index
  dataPrep("mineral",c("ppe_pei.tif","siltcontent_pei.tif","claycontent_pei.tif","organiccarbon_pei.tif","pH_pei.tif","bulkdensity_pei.tif"),
           FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # Prepare organic soil data for the organic soil index
  dataPrep("organic",c("apr_sep_egdd_T5_2001-2020_pei.tif","ppe_pei.tif","bulkdensity_pei.tif","pH_pei.tif"),
           FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))
  # Prepare landscape data for the landscape index
  dataPrep("landscape",c("DEM_pei.tif"),
           FFP(paste0(rasterStackFolder)),FFP(paste0("/data/temp/shapefileAOI.geoJSON")))

  # 2. Indices
  totalFilestemp <- list.files(FFP(paste0("/data/temp/dataTable/")))
  totalFiles <- 0
  for(i in 1:length(totalFilestemp)){
    if(str_contains(totalFilestemp[i],"climate") && str_contains(totalFilestemp[i],".tif")){
      totalFiles <- totalFiles + 1
    }
  }

  # 2a. Climate index
  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/climate_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/climate_table_temp_',i,'.tif')))
    baseClimateRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], "egdd") || str_contains(tempOrder[j], "chu")){
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

    climateResults <- matrix(mapply(climateIndexMain,
                                    cropArrays[1],
                                    cropArrays[2],
                                    cropArrays[3],
                                    get(paste0('climateDF_3'))[3],
                                    get(paste0('climateDF_1'))[3],
                                    get(paste0('climateDF_2'))[3],
                                    get(paste0('climateDF_4'))[3],
                                    cropType),ncol = 1)
    values(baseClimateRaster) <- climateResults
    writePermData(baseClimateRaster,FFP(paste0('/data/temp/results/')),
                  paste0('climateResults_',i),"GTiff")
  }

  # 2b. Mineral soil index
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
  for(i in 1:totalFiles){
    tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/organic_processOrder_',i,'.txt')), header = FALSE, sep = ",")
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/organic_table_temp_',i,'.tif')))
    baseOrganicRaster <- raster(tempDF)
    for(j in 1:length(tempOrder)){
      if(count <= length(tempDF[1])){
        if(str_contains(tempOrder[j], "egdd")){
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
  if(!file.exists(FFP(paste0("/data/results/")))){
    fileLocation <- FFP(paste0("/data/results/"))
    file.create(fileLocation)
  }

  # Stack all the data layers and select the worst result from all the indices
  totalFiles <- list.files(FFP(paste0("/data/temp/results/")))
  baseRaster <- NULL

  for(i in 1:(length(totalFiles)/4)){
    while(i <= length(totalFiles)){
      if(str_contains(paste0("climateResults_",i)) ||
         str_contains(paste0("mineralResults_",i)) ||
         str_contains(paste0("organicResults_",i)) ||
         str_contains(paste0("landscapeResults_",i))){

        if(i == 1){
          tempRasterStack <- loadRaster(FFP(paste0("/data/temp/results/")))
          baseRaster <- raster(tempRasterStack)
        } else {
          tempRasterStack <- stack(tempRasterStack,loadRaster(FFP(paste0("/data/temp/results/"))))
        }
      }
      i <- i + 1
    }

    value(baseRaster) <- mapply(function(w,x,y,z) return(max(w,x,y,z)),raster(tempRasterStack, layer = 1),
                         raster(tempRasterStack, layer = 2),
                         raster(tempRasterStack, layer = 3),
                         raster(tempRasterStack, layer = 4))

    writePermData(baseRaster,FFP(paste0("/data/results/")),paste0("FinalResults_",i,".tif"),"GTiff")
  }

}
