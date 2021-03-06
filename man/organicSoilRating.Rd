% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/organic_soil_index.R
\name{organicSoilRating}
\alias{organicSoilRating}
\title{Organic Soil rating

The organic soil rating calculates the rating class for the organic soil index.}
\usage{
organicSoilRating(
  soilTemp,
  moistureDeduct,
  surfaceFactors,
  subsurfaceFactors,
  drainage
)
}
\arguments{
\item{soilTemp}{The results from the climate factors.}

\item{moistureDeduct}{The results from the moistureDeduct factor.}

\item{surfaceFactors}{The results from the surfaceFactors.}

\item{subsurfaceFactors}{The results from the subsurfaceFactor.}

\item{drainage}{The results from the drainage factor.}
}
\value{
The climate rating.
}
\description{
Organic Soil rating

The organic soil rating calculates the rating class for the organic soil index.
}
