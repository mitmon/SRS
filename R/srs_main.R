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

srsMain <- function(indices,rasterStackFolder,shapefileAOI){

  # 1. Data prep tools
  for(i in 1:length(indices)){
    dataPrep(indices[[i,1]],indices[[i,2]],rasterStackFolder,shapefileAOI)
  }
  # 2. Indices
  # 2a. Climate index
  climateIndexMain(ratingTableArrayMC,ratingTableArrayESM,ratingTableArrayEFM, ppe, temperatureFactor, ppeSpring, ppeFall, type)
  # 2b. Mineral soil index
  mineralIndexMain(ppe,surfaceSiltPercent,surfaceClayPercent,subsurfaceSiltPercent,subsurfaceClayPercent,waterTableDepth,
                   surfaceOC,depthOfTopSoil,surfacepH,surfaceSalinity,surfaceSodicity,depthOfPeat,subsurfaceBulkDensity,
                   impedingLayerDepth,subsurfacepH,subsurfaceSalinity,subsurfaceSodicity)
  # 2c. Organic soil index
  organicIndexMain(egdd,
                   ppe,surfaceBD,subsurfaceBD,depthToWaterTable,
                   surfacepH,surfaceSalinity,
                   subsurfacepH,subsurfaceSalinity)
  # 2d. Landscape index
  landscapeIndexMain(slopePercent,slopeLength,NA, NA, NA)
  # 3. Final rating
  writePermData()
}
