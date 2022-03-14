
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
                                  ratingTableArrayMC,
                                  ratingTableArrayESM,
                                  ratingTableArrayEFM,
                                  get(paste0('climateDF_3'))[3],
                                  get(paste0('climateDF_1'))[3],
                                  get(paste0('climateDF_2'))[3],
                                  get(paste0('climateDF_4'))[3],
                                  "EGDD"),ncol = 1)
  values(baseClimateRaster) <- climateResults
  writePermData(baseClimateRaster,FFP(paste0('/data/temp/results/')),
                paste0('climateResults_',i),"GTiff")
}
