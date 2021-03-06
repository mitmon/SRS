% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/operational_tools.R
\name{batchMaskRaster}
\alias{batchMaskRaster}
\title{Batch Mask Raster}
\usage{
batchMaskRaster(requiredDataArray, inputFolder, exportFolder)
}
\arguments{
\item{requiredDataArray}{The required data for each index, pre-determined in the SRS_main.R file.}

\item{inputFolder}{Input file array.}

\item{exportFolder}{Export folder. Location where data is saved.}
}
\value{
Masked input file
}
\description{
Batch mask an array of input files.
** This tool is meant to be used to mask dead space around two images. This
tool helps with issues of miss-aligned input files by filling the extra space
in the larger of the two input files with with NA data.**
}
