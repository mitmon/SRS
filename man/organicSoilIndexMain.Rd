% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/organic_soil_index.R
\name{organicSoilIndexMain}
\alias{organicSoilIndexMain}
\title{Organic Soil Index Main}
\usage{
organicSoilIndexMain(
  egdd,
  ppe,
  surfaceBD,
  subsurfaceBD,
  depthToWaterTable,
  surfacepH,
  surfaceSalinity,
  subsurfacepH,
  subsurfaceSalinity
)
}
\arguments{
\item{egdd}{Effective growing degree days either T5 or T10.}

\item{ppe}{Precipitation minus potential evapotranspiration}

\item{surfaceBD}{Surface bulk density Mg/m^3}

\item{subsurfaceBD}{The subsurface bulk density Mg/m^3. Default is at 60cm depth.}

\item{depthToWaterTable}{Depth to water table in cm}

\item{surfacepH}{Surface pH measured in saturated paste}

\item{surfaceSalinity}{Surface salinity measured in saturated paste (dS/m)}

\item{subsurfacepH}{The subsurface pH measured in saturated paste (dS/m). Default is at 60cm depth.}

\item{subsurfaceSalinity}{The subsurface salinity measured in saturated paste (dS/m). Default is at 60cm depth.}
}
\description{
The organic soil index main calls all required function and produces the rating
for organic soil over the study site.
}
