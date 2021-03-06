% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crop_library.R
\name{numberCutsComponent}
\alias{numberCutsComponent}
\title{Number of possible cut component}
\usage{
numberCutsComponent(inputArray, egdd)
}
\arguments{
\item{inputArray}{An input array with the upper and lower bounds for the
deduction in each rating.}

\item{egdd}{Effective growing degree days}
}
\value{
deduction value
}
\description{
Calculate the point deduction based on the total number of cuts possible
during the growing season.
}
