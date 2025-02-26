% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_wCV.R
\name{calculate_wCV}
\alias{calculate_wCV}
\title{Calculate Within-Subject Coefficient of Variation (wCV)}
\usage{
{
  calculate_wCV(measurement1, measurement2)
}
}
\arguments{
\item{measurement1}{A numeric vector of the first set of measurements.}

\item{measurement2}{A numeric vector of the second set of measurements.}
}
\value{
A list containing:
\item{WithinSubjectVariance}{The variance within subjects.}
\item{WithinSubjectSD}{The standard deviation within subjects.}
\item{OverallMean}{The overall mean of the measurements.}
\item{wCV}{The Within-Subject Coefficient of Variation, expressed as a percentage.}
\item{Data}{A data frame with detailed intermediate calculations.}
}
\description{
{
This function calculates the Within-Subject Coefficient of Variation (wCV) from two numeric vectors of measurements, each representing repeated measurements for a series of subjects.
}
}
\examples{
{
  measurement1 <- c(100, 102, 98, 101, 99)
  measurement2 <- c(101, 103, 97, 100, 98)
  calculate_wCV(measurement1, measurement2)
}
}
\references{
{
\enumerate{
\item (2015) Quantitative Imaging Biomarkers: A Review of Statistical Methods for Technical Performance Assessment. Stat Methods Med Res 24:27–67. https://doi.org/10.1177/0962280214537344
}
}
}
\author{
{
Giorgio Maria Agazzi, MD \email{giorgiomaria.agazzi@gmail.com}
}
}
\keyword{radiology}
\keyword{repeatability}
\keyword{{}
\keyword{}}
