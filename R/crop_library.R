# Crop library

# This library provides user input or pre-defined data for each crop library.
# The library holds the default input information or takes in the user input.
# There are currently six crops to chose from with future versions including more crops.

# Creation date: Feb 22, 2022
# Last updated: Feb 22, 2022

#' Moisture component
#'
#' Calculate the point deduction based on the moisture component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param ppe Precipitation minus potential evapotranspiration
#' @return deduction value
#' @export
moistureComponent <- function(inputArray,ppe){
  for(i in 1:length(inputArray)){
    if(is.na(ppe) || is.na(inputArray[[i]])){
      return(0)
    } else if(ppe >= inputArray[[i]][1] && ppe < inputArray[[i]][2]){
      # The deduction is limited to 100 points
      if(inputArray[[i]][3] > 100){
        return(100)
      } else {
        return((100- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Effective growing degree days component
#'
#' Calculate the point deduction based on the Effective growing degree days component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param egdd Effective growing degree days
#' @return deduction value
#' @export
egddComponent <- function(inputArray,egdd){
  for(i in 1:length(inputArray)){
    if(is.na(egdd) || is.na(inputArray[[i]])){
      return(0)
    } else if(egdd >= inputArray[[i]][1] && egdd < inputArray[[i]][2]){
      # The deduction is limited to 100 points
      if(inputArray[[i]][3] > 100){
        return(100)
      } else {
        return((100- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Crop heat unit component
#'
#' Calculate the point deduction based on the crop heat unit component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param chu Crop heat unit
#' @return deduction value
#' @export
chuComponent <- function(inputArray,chu){
  for(i in 1:length(inputArray)){
    if(is.na(chu) || is.na(inputArray[[i]])){
      return(0)
    } else if(chu >= inputArray[[i]][1] && chu < inputArray[[i]][2]){
      # The deduction is limited to 100 points
      if(inputArray[[i]][3] > 100){
        return(100)
      } else {
        return((100- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Early spring moisture component
#'
#' Calculate the point deduction based on the early spring moisture component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param ppe Precipitation minus potential evapotranspiration
#' @return deduction value
#' @export
esmComponent <- function(inputArray,ppe){
  for(i in 1:length(inputArray)){
    if(is.na(ppe) || is.na(inputArray[[i]])){
      return(0)
    } else if(ppe >= inputArray[[i]][1] && ppe < inputArray[[i]][2]){
      # The early spring moisture is limited to 10% decrease in productivity
      if(inputArray[[i]][3] > 10){
        return(10)
      } else {
        return((10- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Excess fall moisture component
#'
#' Calculate the point deduction based on the excess fall moisture component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param ppe Precipitation minus potential evapotranspiration
#' @return deduction value
#' @export
efmComponent <- function(inputArray,ppe){
  for(i in 1:length(inputArray)){
    if(is.na(ppe) || is.na(inputArray[[i]])){
      return(0)
    } else if(ppe >= inputArray[[i]][1] && ppe < inputArray[[i]][2]){
      # The excess fall moisture is limited to 10% decrease in productivity
      if(inputArray[[i]][3] > 10){
        return(10)
      } else {
        return((10- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Early fall frost component
#'
#' Calculate the percent deduction based on the early fall frost component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param daysBeforeFrost Number of days before average fall frost.
#' @return deduction value
#' @export
effComponent <- function(inputArray,daysBeforeFrost){
  for(i in 1:length(inputArray)){
    if(is.na(daysBeforeFrost) || is.na(inputArray[[i]])){
      return(0)
    } else if(daysBeforeFrost >= inputArray[[i]][1] && daysBeforeFrost < inputArray[[i]][2]){
      # The early fall frost is limited to 10% decrease in productivity
      if(inputArray[[i]][3] > 10){
        return(10)
      } else {
        return((10- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Surface salinity component
#'
#' Calculate the point deduction based on the surface salinity component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param ec Electro-conductivity
#' @return deduction value
#' @export
surfaceSalinityComponent <- function(inputArray,ec){
  for(i in 1:length(inputArray)){
    if(is.na(ec) || is.na(inputArray[[i]])){
      return(0)
    } else if(ec >= inputArray[[i]][1] && ec < inputArray[[i]][2]){
      # The deduction is limited to 100 points
      if(inputArray[[i]][3] > 100){
        return(100)
      } else {
        return((100- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Subsurface salinity component
#'
#' Calculate the point deduction based on the subsurface salinity component
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param ec Electro-conductivity
#' @return deduction value
#' @export
subsurfaceSalinityComponent <- function(inputArray,ec){
  for(i in 1:length(inputArray)){
    if(is.na(ec) || is.na(inputArray[[i]])){
      return(0)
    } else if(ec >= inputArray[[i]][1] && ec < inputArray[[i]][2]){
      # The subsurface salinity component is limited to 70% decrease in productivity
      if(inputArray[[i]][3] > 70){
        return(70)
      } else {
        return((70- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

#' Number of possible cut component
#'
#' Calculate the point deduction based on the total number of cuts possible
#' during the growing season.
#' @param inputArray An input array with the upper and lower bounds for the
#' deduction in each rating.
#' @param egdd Effective growing degree days
#' @return deduction value
#' @export
numberCutsComponent <- function(inputArray,egdd){
  for(i in 1:length(inputArray)){
    if(is.na(egdd) || is.na(inputArray[[i]])){
      return(0)
    } else if(egdd >= inputArray[[i]][1] && egdd < inputArray[[i]][2]){
      # The deduction is limited to 100 points
      if(inputArray[[i]][3] > 100){
        return(100)
      } else {
        return((100- as.numeric(inputArray[[i]][3])))}
    } else if(i == length(inputArray)){
      return(0)
    } else {
      next
    }
  }
}

################################################################################
# Future updates.
#' User defined crop input
#'
#' The user can define their own crop parameters. This function inputs the user
#' data from the server or from a CSV template file.
#' @param inputData
#' @return
#' @export

