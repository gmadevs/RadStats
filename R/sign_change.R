#' Check Significant Change
#'
#' This function evaluates whether the relative change in tissue-normalized AUC between a baseline and follow-up measurement is statistically significant.
#'
#' @param Y1 Numeric. Baseline tissue-normalized AUC.
#' @param Y2 Numeric. Follow-up tissue-normalized AUC.
#' @param wCV Numeric. Within-subject coefficient of variation (as a decimal, e.g., 0.05 for 5%).
#'
#' @return A character string indicating:
#' - "Significant increase"
#' - "Significant decrease"
#' - "No significant change"
#'
#' @details
#' The function computes the relative change:
#'
#' \deqn{\text{Relative Change} = \frac{Y_2 - Y_1}{Y_1}}
#'
#' It evaluates this change against thresholds for significant increase and decrease:
#'
#' \deqn{k = 1.96 \times wCV}
#'
#' Thresholds:
#' - Increase: \deqn{\frac{k^2 + k \sqrt{2 - k^2}}{1 - k^2}}
#' - Decrease: \deqn{\frac{k^2 - k \sqrt{2 - k^2}}{1 - k^2}}
#'
#' @references
#' 1. Shiroishi MS, Erickson BJ, Hu LS, et al. The QIBA Profile for Dynamic Susceptibility Contrast MRI Quantitative Imaging Biomarkers for Assessing Gliomas. Haller S, ed. Radiology 2024;313:e232555.
#'
#' @examples
#' # Example data
#' Y1 <- 100  # Baseline AUC
#' Y2 <- 110  # Follow-up AUC
#' wCV <- 0.05  # Coefficient of variation
#'
#' # Check significance
#' check_sign(Y1, Y2, wCV)
#'
#' @export
check_sign <- function(Y1, Y2, wCV) {
  k <- 1.96 * wCV
  increase_threshold <- (k^2 + k * sqrt(2 - k^2)) / (1 - k^2)
  decrease_threshold <- (k^2 - k * sqrt(2 - k^2)) / (1 - k^2)
  relative_change <- (Y2 - Y1) / Y1

  if (relative_change > increase_threshold) {
    return("Significant increase")
  } else if (relative_change < decrease_threshold) {
    return("Significant decrease")
  } else {
    return("No significant change")
  }
}
