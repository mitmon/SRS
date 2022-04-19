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
library(SRS.6.3.0)

# Set params
args = commandArgs(trailingOnly=TRUE)
cropType <- args[1] #cropType
rasterStackFolder <- args[2] #rasterStackFolder
indices <- args[3] #indices
shapefileAOI <- args[4] #shapefileAOI

serverPrep <- function(cropType,indices,rasterStackFolder,shapefileAOI){

  # 1. Convert geoJSON string to geoJSON.
  if(file.exists(FFP(paste0("/data/shapefileAOI.geoJSON")))){
    fileLocation <- FFP(paste0("/data/shapefileAOI.geoJSON"))
    file.remove(fileLocation)
  }
  fileLocation <- FFP(paste0("/data/shapefileAOI.geoJSON"))
  file.create(fileLocation)
  writeLines(shapefileAOI,fileLocation)

  # Convert from geoJSON to shapefile
  shapefileAOI <- geojson_sf(fileLocation)
  st_write(shapefileAOI,dsn = FFP(paste0('/data/shapefile.shp')), driver = 'ESRI Shapefile', delete_layer = TRUE)
  shapefileAOI <- st_read(FFP(paste0('/data/shapefile.shp')))
  shapefileAOI <- st_transform(shapefileAOI, "+proj=longlat +datum=WGS84 +no_defs")
  st_write(shapefileAOI,dsn = FFP(paste0('/data/shapefile.shp')), driver = 'ESRI Shapefile', delete_layer = TRUE)
  shapefileAOI <- FFP(paste0('/data/shapefile.shp'))

  # 2. Convert the indices array string into an array to be used later
  # function to convert nested list string into nested list
  strNL_to_NL <- function(str) {
    # change the "," between the lists for easy split
    strv2 <- gsub(",\\[", "+[", str)
    temp <- unlist(strsplit(strv2, "\\+"))

    nl <- list() #nested list
    for(i in 1:length(temp)){
      li <- c() # list in nested list
      temp2 <- unlist(strsplit(temp[i], ","))
      # convert string into number & append to list
      for(j in 1:length(temp2)){
        if (grepl( "[", temp2[j], fixed = TRUE))
          li <- c(li, as.numeric(substr(temp2[j],2,nchar(temp2[j]))))
        else if (grepl( "]", temp2[j], fixed = TRUE))
          li <- c(li, as.numeric(substr(temp2[j],1,nchar(temp2[j]) - 1)))
        else
          li <- c(li, as.numeric(temp2[j]))
      }

      nl[[i]] <- li # append list in nested list
    }
    return(nl)
  }

  ratingTableArrays <- unlist(strsplit(indices, "~"))
  data <- list()
  for(i in 2:length(ratingTableArrays)){
    if(i %% 2 == 0){
      # key <- ratingTableArrays[i]
      # start at 3rd charater(2nd "[") cuz ":[[min,max,PD]...]"
      valStr <- substr(ratingTableArrays[i+1], 3, nchar(ratingTableArrays[i+1]) - 2)
      # nested list string -> nested list
      # l <- list(append(key, strNL_to_NL(valStr)))
      data <- append(data, strNL_to_NL(valStr))
    }
  }

  data <- matrix(data,ncol = 7)
  indicesCalc <- list("climate","mineral","organic","landscape")
  # saveLocation <- paste0(unlist(strsplit(getwd(), "backend_stuff"))[1], 'backend_stuff/temp')
  saveLocation <- '~/Library/Mobile Documents/com~apple~CloudDocs/Desktop - iCloud/AAFC/SRS_V6_3/SRS.6.3.0/data/results/'
  # print(data)

  # # 3. Run the main function
  srsMain(cropType,data,rasterStackFolder,shapefileAOI,indicesCalc,saveLocation)

}

serverPrep(cropType,indices,rasterStackFolder,shapefileAOI)
