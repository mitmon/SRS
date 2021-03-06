% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mineral_soil_index.R
\name{drainageDeduction}
\alias{drainageDeduction}
\title{Drainage Deduction}
\usage{
drainageDeduction(depthToWaterTable, ppe, hydraulicCond)
}
\arguments{
\item{depthToWaterTable}{Depth to water table in cm (Highest 20-day average in
growing season).}

\item{ppe}{Precipitation minus potential evapotranspiration.}

\item{hydraulicCond}{Hydraulic conductivity (cm/h)}
}
\value{
Percentage deduction for drainage.
}
\description{
The drainage deduction is used to evaluate the soil properties which include
the water table and hydraulic conductivity.The rating is based principally on
management or traffic ability considerations. Three is one parameter for
drainage. This parameter determines the percentage deduction for the soil
regime. Currently this parameter is not used in the calculations with potential
for future version to include the drainage.
}
