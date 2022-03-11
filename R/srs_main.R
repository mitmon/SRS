# SRS Main

# This library provides the main function body for the Agriculture and
# Agri-Foods Canada's suitability rating system.

# Creation date: Mar 06, 2022
# Last updated: Mar 06, 2022

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

srsMain <- function(indices,rasterStackFolder,shapefileAOI){

  # 1. Data prep tools
  for(i in 1:length(indices)){
    dataPrep(indices[[i,1]],indices[[i,2]],rasterStackFolder,shapefileAOI)
    if(i == length(indices)){
      totalFiles <- i
    }
  }
  # 2. Indices
  # 2a. Climate index
  for(i in 1:totalFiles){
    tempFile <- read.delim(FFP(paste0('/data/temp/dataTable/climate_processOrder_',i,'.txt')), header = FALSE, sep = ",")
  }
  climateIndexMain(ratingTableArrayMC,ratingTableArrayESM,ratingTableArrayEFM, ppe, temperatureFactor, ppeSpring, ppeFall, type)
  # 2b. Mineral soil index
  mineralIndexMain(ppe,surfaceSiltPercent,surfaceClayPercent,subsurfaceSiltPercent,subsurfaceClayPercent,waterTableDepth,
                   surfaceOC,depthOfTopSoil,surfacepH,surfaceSalinity,surfaceSodicity,depthOfPeat,subsurfaceBulkDensity,
                   impedingLayerDepth,subsurfacepH,subsurfaceSalinity,subsurfaceSodicity)
  # 2c. Organic soil index
  tempOrder <- read.delim(FFP(paste0('/data/temp/dataTable/organic_processOrder_',i,'.txt')), header = FALSE, sep = ",")

  for(i in 1:totalFiles){
    count <- 1
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/organic_table_temp_',i,'.tif')))
    baseOrganicRaster <- raster(tempDF)
    for(j in 1:length(tempDF[1])){
      if((j %% 2) != 0){
        if(str_contains(tempOrder[count], "egdd")){
          temp <- raster(tempDF, layer = j)
          assign(paste0("organicDF_1"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[count], "ppe")){
          temp <- raster(tempDF, layer = j)
          assign(paste0("organicDF_2"),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
          count <- count + 1
        } else if(str_contains(tempOrder[count], "bulkdensity")){
          temp <- raster(tempDF, layer = j)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_3"),temp)
          temp <- raster(tempDF, layer = (j+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_4"),temp)
          count <- count + 1
        } else if(str_contains(tempOrder[count], "pH")){
          temp <- raster(tempDF, layer = j)
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_5"),temp)
          temp <- raster(tempDF, layer = (j+1))
          temp <- as.data.frame(temp, xy = TRUE, rm.na = FALSE)
          assign(paste0("organicDF_6"),temp)
          count <- count + 1
        }
      } else {
        next
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
    writePermData(baseOrganicRaster,'~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/AAFC/SRS_V6_3/SRS.6.3.0/data/temp/results/',
                  paste0('organicResults_',i),"GTiff")
  }
  # 2d. Landscape index
  for(i in 1:totalFiles){
    tempDF <- loadRaster(FFP(paste0('/data/temp/dataTable/landscape_table_temp_',i,'.tif')))
    baseOrganicRaster <- raster(tempDF)
    for(j in 1:length(tempDF[1])){
      temp <- raster(tempDF, layer = j)
      assign(paste0("landscapeDF_",j),as.data.frame(temp, xy = TRUE, rm.na = FALSE))
      # In future versions, allow for adjustable number of input parameters.
    }
    landscapeResults <- matrix(mapply(landscapeIndexMain, get(paste0('landscapeDF_1'))[3],get(paste0('landscapeDF_2'))[3],NA,NA,NA),ncol = 1)
    values(baseOrganicRaster) <- landscapeResults
    writePermData(baseOrganicRaster,'~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/AAFC/SRS_V6_3/SRS.6.3.0/data/temp/results/',
                  paste0('landscapeResults_',i),"GTiff")
  }
  # 3. Final rating
  writePermData()
}
