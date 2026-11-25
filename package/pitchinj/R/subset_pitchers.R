#' Subset Pitchers with Complete Data
#'
#' Creates subsets of pitchers from a dataset based on level (majors or minors) 
#' and availability of feature data. Returns player-year combinations with complete 
#' data for specified groups of features.
#'
#' @param data A data frame containing pitcher information, including at least:
#' \describe{
#'   \item{\code{player_id}}{Unique pitcher identifier.}
#'   \item{\code{year}}{Season year.}
#'   \item{\code{level}}{Competition level (e.g., "majors", "minors").}
#'   \item{Feature columns required by \code{get_features()}, such as demographics, 
#'   ballistics, and biomechanics variables.}
#' }
#' @param reboot_glossary a table of Reboot documentation from
#'   \code{\link{pitchinj::get_reboot_metrics()}}
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{\code{majors}}{Player-year combinations for major-league pitchers with complete 
#'   demographics and ballistics features.}
#'   \item{\code{minors}}{Player-year combinations for minor-league pitchers (year > 2020) 
#'   with complete demographics and ballistics features.}
#'   \item{\code{full}}{The union of the majors and minors cohorts.}
#' }
#'
#' @details
#' Feature sets are determined by \code{get_features()}, which selects columns corresponding 
#' to categories of variables (e.g., "demographics", "ballistics", "biomechanics").  
#' Rows with missing values in the required feature sets are removed before subsetting.
#'
#' @examples
#' \dontrun{
#' pitcher_subsets <- subset_pitchers(pitcher_data)
#' pitcher_subsets$majors   # majors with complete ballistics + demographics
#' pitcher_subsets$minors   # minors (year > 2020) with complete ballistics + demographics
#' pitcher_subsets$biomech  # pitchers with complete biomechanics
#' }
#'
#' @seealso \code{\link{get_features}}
#'
#' @export
subset_pitchers <- function(data, reboot_glossary, verbose = FALSE) {

  if (verbose) {
    cat(glue::glue("A+/AA/AAA/MLB pitcher-seasons 2018-2025: {nrow(data)}"), "\n")
  }

  majors_cohort <- data |>
    dplyr::filter(level == "MLB", year > 2017)
  if (verbose) {
    cat(glue::glue("MLB pitcher-seasons 2018-2025: {nrow(majors_cohort)}"), "\n")
  }
 
  majors_cohort <- majors_cohort |>
    dplyr::filter(!is.na(velo_primary))
  if (verbose) {
    cat(glue::glue("MLB pitcher-seasons 2018-2025 with tracking: {nrow(majors_cohort)}"), "\n")
  }

  minors_cohort <- data |>
    dplyr::filter(level %in% c("A+", "AA", "AAA"), year > 2021)
  if (verbose) {
    cat(glue::glue("A+/AA/AAA pitcher-seasons 2022-2025: {nrow(minors_cohort)}"), "\n")
  }

  minors_cohort <- minors_cohort |>
    dplyr::filter(!is.na(velo_primary))
  if (verbose) {
    cat(glue::glue("A+/AA/AAA pitcher-seasons 2022-2025 with tracking: {nrow(minors_cohort)}"), "\n")
  }

  full_cohort <- dplyr::bind_rows(majors_cohort, minors_cohort)
  if (verbose) {
    cat(glue::glue("A+/AA/AAA/MLB pitcher-seasons 2022-2025 with tracking: {nrow(full_cohort)}"), "\n")
  }

  pitcher_subset <- list(
    full = dplyr::select(full_cohort, player_id, year),
    majors = dplyr::select(majors_cohort, player_id, year),
    minors = dplyr::select(minors_cohort, player_id, year)
  )

  return(pitcher_subset)
}
