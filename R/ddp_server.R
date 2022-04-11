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
  saveLocation <- '~/Library/Mobile Documents/com~apple~CloudDocs/Desktop - iCloud/AAFC/SRS_V6_3/SRS.6.3.0/data/'

  # 3. Run the main function
  srsMain(cropType,data,rasterStackFolder,shapefileAOI,indicesCalc,saveLocation)

}
