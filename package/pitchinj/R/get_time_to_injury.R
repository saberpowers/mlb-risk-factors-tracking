#' Calculate Cumulative Pitch Count before First Injury
#'
#' This function computes the total number of pitches thrown by each pitcher in a given year up to
#' the date of their first listed injury. If no injury is recorded for a pitcher-year, the
#' cumulative pitch count for the season is returned with a severity of `"none"`.
#'
#' @param injury_data A data frame containing injury events with columns `player_id`, `year`,
#'   `date`, and `severity`. Each row represents an injury occurrence.
#' @param pitcher_game_log A data frame containing pitcher game logs with columns
#'   `pitcher` (player ID), `year`, `game_date`, and `pitch_count`.
#'   Each row represents a single game.
#'
#' @return A data frame with one row per pitcher-year combination containing:
#' \describe{
#'   \item{player_id}{The unique identifier of the pitcher.}
#'   \item{year}{The season year.}
#'   \item{severity}{The severity of the first listed injury, or `"none"` if no injury occurred.}
#'   \item{pitch_count}{
#'     The cumulative number of pitches thrown up to (and including) the date of the first injury
#'     or through the full season if uninjured.
#'   }
#' }
#'
#' @export
get_time_to_injury <- function(injury_data, pitcher_game_log) {

  # Identify each pitcher-season as coming from the Major leagues or the minors leagues.
  # This is only used for reporting pitcher-season level when it's not in the injury data.
  pitcher_level_game_log <- pitcher_game_log |>
    # On very rare occasion, pitch count is missing, so we treat it as zero
    dplyr::mutate(pitch_count = dplyr::coalesce(pitch_count, 0)) |>
    dplyr::group_by(player_id, year, level = ifelse(Lvl == "MLB", "majors", "minors")) |>
    dplyr::summarize(pitch_count = sum(pitch_count), .groups = "drop") |>
    dplyr::group_by(player_id, year) |>
    dplyr::arrange(-pitch_count) |>
    dplyr::slice(1) |>
    dplyr::select(player_id, year, level)

  pitch_count_by_date <- pitcher_game_log |>
    # On very rare occasion, pitch count is missing, so we treat it as zero
    dplyr::mutate(pitch_count = dplyr::coalesce(pitch_count, 0)) |>
    dplyr::group_by(player_id, year) |>
    dplyr::arrange(game_date) |>
    dplyr::mutate(cumulative_pitch_count = cumsum(pitch_count)) |>
    dplyr::select(player_id, year, game_date, cumulative_pitch_count) |>
    dplyr::left_join(pitcher_level_game_log, by = c("player_id", "year"))

  first_injury_date <- injury_data |>
    dplyr::group_by(player_id, year) |>
    dplyr::arrange(date) |>
    dplyr::slice(1) |>
    dplyr::select(player_id, year, level, date, severity)

  time_to_injury <- pitch_count_by_date |>
    dplyr::full_join(
      y = first_injury_date,
      by = c("player_id", "year"),
      suffix = c("_game_log", "_injury_data")
    ) |>
    dplyr::mutate(
      level = dplyr::coalesce(level_game_log, level_injury_data),
      severity = dplyr::coalesce(severity, "none"),
      # only count pitches thrown before first injury list placement
      cumulative_pitch_count_pre_injury = dplyr::case_when(
        is.na(cumulative_pitch_count) ~ 0,
        game_date > date ~ 0,
        TRUE ~ cumulative_pitch_count
      )
    ) |>
    dplyr::group_by(player_id, year, level, severity) |>
    dplyr::summarize(pitches = max(cumulative_pitch_count_pre_injury), .groups = "drop")

  return(time_to_injury)
}
