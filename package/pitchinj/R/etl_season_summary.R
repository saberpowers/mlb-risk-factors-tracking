#' Extract, Transform, and Load Pitcher Season Summary Data
#'
#' Reads and processes pitcher season-level data from a CSV file. Aggregates statistics by player
#' and year, assigns level and role, and summarizes workload metrics.
#'
#' @return A data frame with one row per pitcher-year, containing the following columns:
#' \describe{
#'   \item{year}{Integer. Season year.}
#'   \item{player_id}{Integer. Unique player identifier.}
#'   \item{level}{Character. Highest level reached by the player that season (e.g., "MLB", "AAA").}
#'   \item{games}{Integer. Total number of games played.}
#'   \item{batters_faced}{Integer. Total batters faced in the season.}
#'   \item{pitches}{Integer. Total number of pitches thrown.}
#'   \item{role}{Factor. Assigned pitcher role based on average pitches per game.
#'     Either `"starter"` or `"reliever"`, with `"starter"` as the reference level.
#'   }
#' }
#'
#' @details
#' The role classification is based on pitch count per game:
#' \itemize{
#'   \item `"starter"`: \code{pitches / games >= 75}, or \code{>= 60} for non-MLB levels.
#'   \item `"reliever"`: All other cases.
#' }
#' The data is read from `"data/pitcher_season_summary.csv"` and grouped to ensure one record per
#' pitcher per year.
#'
#' @examples
#' season_summary <- etl_season_summary()
#' dplyr::count(season_summary, role)
#'
#' @export
etl_season_summary <- function() {

  season_summary <- data.table::fread("input/data/pitcher_season_summary.csv") |>
    # Aggregate across leagues within level before determining level
    dplyr::group_by(year, level, player_id) |>
    dplyr::summarize(
      games = sum(games),
      batters_faced = sum(batters_faced),
      pitches = sum(pitches),
      .groups = "drop"
    ) |>
    dplyr::group_by(year, player_id) |>
    dplyr::arrange(-batters_faced) |>
    dplyr::summarize(
      level = level[1],
      games = sum(games),
      batters_faced = sum(batters_faced),
      pitches = sum(pitches),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      role = dplyr::case_when(
        pitches / games >= 75 ~ "starter",
        pitches / games >= 60 & level != "MLB" ~ "starter",
        TRUE ~ "reliever"
      ) |>
        factor(levels = c("starter", "reliever"))   # we want starter to be the "default" level
    )

  return(season_summary)
}
