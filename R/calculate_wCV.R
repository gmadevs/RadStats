#' @name calculate_wCV
#' @title Calculate Within-Subject Coefficient of Variation (wCV)
#' @description{
#'   This function calculates the Within-Subject Coefficient of Variation (wCV) from two numeric vectors of measurements, each representing repeated measurements for a series of subjects.
#' }
#' @usage{
#'   calculate_wCV(measurement1, measurement2)
#' }
#' @param measurement1 A numeric vector of the first set of measurements.
#' @param measurement2 A numeric vector of the second set of measurements.
#' @return A list containing:
#'   \item{WithinSubjectVariance}{The variance within subjects.}
#'   \item{WithinSubjectSD}{The standard deviation within subjects.}
#'   \item{OverallMean}{The overall mean of the measurements.}
#'   \item{wCV}{The Within-Subject Coefficient of Variation, expressed as a percentage.}
#'   \item{Data}{A data frame with detailed intermediate calculations.}
#' @examples{
#'   measurement1 <- c(100, 102, 98, 101, 99)
#'   measurement2 <- c(101, 103, 97, 100, 98)
#'   calculate_wCV(measurement1, measurement2)
#' }
#' @references{
#' 1. (2015) Quantitative Imaging Biomarkers: A Review of Statistical Methods for Technical Performance Assessment. Stat Methods Med Res 24:27–67. https://doi.org/10.1177/0962280214537344
#' }
#' @author{
#' Giorgio Maria Agazzi, MD \email{giorgiomaria.agazzi@gmail.com}
#' }
#' @keywords{
#' radiology
#' repeatability
#' }

# Function to calculate Within-Subject Coefficient of Variation (wCV)
calculate_wCV <- function(measurement1, measurement2) {
  # Ensure the inputs are numeric and of the same length
  if (length(measurement1) != length(measurement2)) {
    stop("The two input vectors must have the same length.")
  }

  # Create a data frame for internal calculations
  data <- data.frame(
    Subject = 1:length(measurement1),
    Measurement1 = measurement1,
    Measurement2 = measurement2
  )

  # Calculate the difference and squared differences
  data$Difference <- data$Measurement1 - data$Measurement2
  data$SquaredDifference <- data$Difference^2

  # Calculate the within-subject variance
  N <- nrow(data) * 2  # Total number of measurements
  S <- nrow(data)      # Number of subjects
  var_within <- sum(data$SquaredDifference) / (N - S)

  # Calculate the within-subject standard deviation
  sd_within <- sqrt(var_within)

  # Calculate the overall mean
  mean_value <- mean(c(data$Measurement1, data$Measurement2))

  # Calculate the wCV
  wCV <- (sd_within / mean_value) * 100

  # Return the results
  result <- list(
    WithinSubjectVariance = var_within,
    WithinSubjectSD = sd_within,
    OverallMean = mean_value,
    wCV = wCV,
    Data = data
  )

  cat("Combined within-subject variance:", round(result$WithinSubjectVariance, 2), "\n")
  cat("Within-subject standard deviation:", round(result$WithinSubjectSD, 2), "\n")
  cat("Overall mean:", round(result$OverallMean, 2), "\n")
  cat("Within-Subject Coefficient of Variation (wCV):", round(result$wCV, 2), "%\n")

}
