#' Standardize Data Columns for Analysis
#'
#' Centers and scales selected columns of a dataset based on feature specifications
#' defined by \code{pitchinj::get_features()}.  
#' Columns flagged as "centered only" are mean-centered, and columns flagged as 
#' "scaled" are divided by their standard deviation.  
#' This transformation is often used prior to modeling to improve numerical stability 
#' and interpretability of coefficients.
#'
#' @param data A data frame containing numeric columns to be standardized.
#' @param ... additional arugments to \code{\link[pitchinj]{get_features}}
#'
#' @return A data frame of the same dimensions as \code{data}, but with selected 
#' columns centered or scaled. Columns not flagged for transformation are unchanged.
#'
#' @details
#' \code{pitchinj::get_features(only_centered = TRUE)} returns the names of 
#' columns that should be centered but not scaled, whereas 
#' \code{pitchinj::get_features(only_scaled = TRUE)} returns the names of 
#' columns that should be scaled by their standard deviation.
#'
#' @seealso \code{\link[pitchinj]{get_features}}
#'
#' @examples
#' \dontrun{
#' # Standardize selected columns based on pitchinj feature specification
#' standardized <- standardize_data(mydata)
#' }
#'
#' @export
standardize_data <- function(data, ...) {
  
  data_standardized <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::any_of(get_features(only_centered = TRUE, ...)),
        .fns = \(x) x - mean(x, na.rm = TRUE)
      ),
      dplyr::across(
        .cols = dplyr::any_of(get_features(only_scaled = TRUE, ...)),
        .fns = \(x) x / sd(x, na.rm = TRUE)
      )
    )

  return(data_standardized)
}