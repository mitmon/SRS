# Server

# This controls the server passing.

# Creation date: Mar 18, 2022
# Last updated: Mar 18, 2022

#' Server passing function
#'
#' This function calls the data preparation tools and climate, mineral soil,
#' organic soil, and landscape indices.
#' @param cropType The input crop type as a string. Options are alfalfa, corn,
#' potatoes, SSSG (spring seeded small grain), soybeans and one more.
#' @return Tiff file with the final results.
#'

serverPrep <- function(cropType,indices,rasterStackFolder,shapefileAOI){

  # 1. Convert geoJSON string to shapefile.
  if(!file.exists(FFP(paste0("/data/temp/shapefileAOI.geoJSON")))){
    fileLocation <- FFP(paste0("/data/temp/shapefileAOI.geoJSON"))
    file.create(fileLocation)
    writeLines(shapefileAOI,fileLocation)
  } else {
    fileLocation <- FFP(paste0("/data/temp/shapefileAOI.geoJSON"))
    file.remove(fileLocation)
    file.create(fileLocation)
    writeLines(shapefileAOI,fileLocation)
  }

  # 2. Convert the indices array string into an array to be used later
  ratingTableArrays <- str_split(indices,"~", simplify = TRUE)
  for(i in 1:length(ratingTableArrays)){
    if(i != 1 && (i %% 2 == 0)){
      tempSplit <- str_split(ratingTableArrays[i],"")
      # tempSplit <- substr(tempSplit[3],2,nchar(tempSplit[3])-1)
      tempSplit <- tempSplit[[1]][-1]
      tempSplit <- tempSplit[-(length(tempSplit))]
      # tempSplit <-
    }
  }
  ratingTableArrays <- substr(ratingTableArrays[3],2,nchar(ratingTableArrays[3])-1)
  ratingTableArrayMC <- 1
  ratingTableArrayESM <- 1
  ratingTableArrayEFM <- 1

  # 3. Run the main function
  srsMain(cropType,indices,rasterStackFolder,shapefileAOI)

}

#   {"Potential evapotranspiration in growing season (Apr)":[[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]],
#     "Potential evapotranspiration in growing season (May-Aug)":[[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]],
#     "Mean min temperature by day":[[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]}

