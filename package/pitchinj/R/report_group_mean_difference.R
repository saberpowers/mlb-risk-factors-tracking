#' Report Group Mean Differences Between Injured and Healthy Individuals
#'
#' Computes and reports summary statistics comparing a set of features between
#' injured and healthy individuals. For each specified feature, the function calculates
#' the mean and standard deviation for both the healthy group (\code{injury == "none"})
#' and the injured group (\code{injury != "none"}), and reports a p-value from a
#' linear regression testing for mean differences.
#'
#' @param data A data frame that includes an \code{injury} column and the features to be analyzed.
#' The \code{injury} column must be a character or factor indicating whether a subject is injured.
#' @param features A character vector of column names in \code{data} to be compared between groups.
#'
#' @return A data frame with one row per feature, containing:
#' \describe{
#'   \item{\code{feature}}{The feature name (as a factor, preserving input order).}
#'   \item{\code{healthy_mean}}{Mean value of the feature among healthy individuals.}
#'   \item{\code{healthy_sd}}{Standard deviation of the feature among healthy individuals.}
#'   \item{\code{injured_mean}}{Mean value of the feature among injured individuals.}
#'   \item{\code{injured_sd}}{Standard deviation of the feature among injured individuals.}
#'   \item{\code{p_value}}{Formatted p-value (as a percentage string) from a linear model comparing groups.}
#'   \item{\code{variable}}{Feature name as a character string (duplicated from \code{feature}).}
#' }
#'
#' @details
#' This function reshapes the data to long format and performs group-wise comparisons
#' using a simple linear model: \code{value ~ injury != "none"}.
#' The p-value reflects the significance of the difference in means between groups.
#'
#' A helper function \code{format_percentage()} must be available in the environment.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' df <- data.frame(
#'   severity = rep(c("none", "short", "long"), each = 10),
#'   strength = rnorm(30),
#'   flexibility = rnorm(30)
#' )
#' report_group_mean_difference(df, features = c("strength", "flexibility"))
#' }
#'
#' @export
report_group_mean_difference <- function(data, features) {
  data |>
    dplyr::mutate(
      status = dplyr::case_when(
        severity != "none" ~ "injured",
        pitches > 0 ~ "healthy"
      )
    ) |>
    # We want to remove any non-injured pitchers who threw 0 pitches (neither healthy nor injured)
    dplyr::filter(!is.na(status)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(features), names_to = "feature") |>
    dplyr::group_by(
      # use factors to maintain ordering of groups
      feature = factor(feature, levels = unique(feature))
    ) |>
    dplyr::summarize(
      healthy_mean = mean(value[status == "healthy"]),
      healthy_sd = sd(value[status == "healthy"]),
      injured_mean = mean(value[status == "injured"]),
      injured_sd = sd(value[status == "injured"]),
      p_value = summary(lm(value ~ status == "injured"))$coefficients[2, 4],
      .groups = "drop"
    ) |>
    dplyr::mutate(variable = as.character(feature))
}
