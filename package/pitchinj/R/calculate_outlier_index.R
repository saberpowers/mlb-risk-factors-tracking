#' Calculate Outlier Index
#'
#' Computes an outlier index for numeric values based on the interquartile range (IQR).
#' Values outside the IQR receive a positive score reflecting their standardized distance
#' beyond the nearest quartile, while values within the IQR receive 0.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector of outlier index values, with larger values indicating more
#' extreme outliers.
#'
#' @examples
#' calculate_outlier_index(c(1, 2, 3, 100))
#'
#' @export
calculate_outlier_index <- function(x) {
  quartile_1 <- quantile(x, probs = 0.25, na.rm = TRUE)
  quartile_3 <- quantile(x, probs = 0.75, na.rm = TRUE)
  interquartile_range <- quartile_3 - quartile_1
  outlier_index_above <- pmax(x - quartile_3, 0) / interquartile_range
  outlier_index_below <- pmax(quartile_1 - x, 0) / interquartile_range
  outlier_index <- pmax(outlier_index_above, outlier_index_below)
  return(outlier_index)
}
