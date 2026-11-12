#' Wrangle Pitcher Data for Year-Over-Year Injury Prediction
#'
#' @description
#' This function integrates several data sources (biomechanics, pitch tracking, 
#' injury lists, and game logs) into a single pitcher-season level dataset. 
#' It specifically lags performance and biomechanical metrics by one year 
#' to allow for the prediction of current-year injury outcomes based on 
#' previous-year data.
#'
#' @param player a table of player data with primary key (player_id), including columns
#' `throw_hand`, `birth_date` and `primary_position`
#' @param data_dir A character string specifying the path to the directory 
#' containing the required CSV files (`player.csv` and `pitcher_game_log.csv`). 
#' Defaults to `"input/data"`.
#' @param biomechanics_data a table of biomechanics data with primary key (player_id, year)
#' from `pitchinj::etl_biomechanics()` (in the `biomechanics` element of the list).
#' @param biomechanics_metrics a character vector of biomechanics features to include in the
#' aggregated data table.
#'
#' @details
#' The function performs several key steps:
#' \itemize{
#'   \item \bold{Data Loading:} Reads local CSVs and calls several \code{pitchinj} 
#'   ETL functions to fetch injury and biomechanical data.
#'   \item \bold{Injury Processing:} Merges standard Injured List (IL) data with 
#'   UCL surgery records, coalescing missing values to determine injury severity 
#'   and pitch counts prior to injury.
#'   \item \bold{Feature Engineering:} 
#'   \itemize{
#'     \item Scales pitch usage by a factor of 10.
#'     \item Calculates composite metrics like \code{horz_gloveside} and 
#'     \code{horz_armside}.
#'     \item Derives "effort" metrics for velocity and spin.
#'     \item Computes pitcher age as of July 1st of the season year.
#'   }
#'   \item \bold{Year-Over-Year Lagging:} Predictor variables from pitch tracking 
#'   and biomechanics are joined to the following year's injury outcomes 
#'   (i.e., \code{year = year + 1}).
#'   \item \bold{Filtering:} The final dataset is restricted to primary pitchers, 
#'   specific professional levels (A+ to MLB), and seasons from 2018 onwards.
#' }
#'
#' @return A \code{data.frame} (tibble) where each row represents a pitcher-season, 
#' containing demographic info, lagged performance metrics, and injury status.
#' 
#' @importFrom data.table fread
#' @importFrom dplyr filter select inner_join full_join mutate across starts_with 
#' coalesce left_join arrange group_by ungroup pull all_of
#' @importFrom tidyr replace_na fill
#' @importFrom lubridate interval years
#' @importFrom glue glue
#' 
#' @export
wrangle_data_year_over_year <- function(player,
                                        data_dir = "input/data",
                                        pitch_tracking_data = NULL,
                                        biomechanics_data = NULL,
                                        biomechanics_metrics = NULL) {

  # Read data from file ----
  
  season_summary <- pitchinj::etl_season_summary()
  injury_hits <- pitchinj::etl_hits(min_days_until_clearance = 7)
  injury_list <- pitchinj::etl_injury_list()
  ucl_surgery <- pitchinj::etl_ucl_surgery()
  pitcher_game_log <- data.table::fread(file.path(data_dir, "pitcher_game_log.csv")) |>
    dplyr::rename(pitch_count = P_TOTAL_PITCHES, starter_flag = P_STARTING_P) |>
    dplyr::filter(year >= 2018)   # match injury list data, which begins in 2018
 
  
  # Wrangle data ----
   
  time_to_injury_hits <- pitchinj::get_time_to_injury(
    injury_data = injury_hits,
    pitcher_game_log = pitcher_game_log
  )
 
  time_to_injury_list <- pitchinj::get_time_to_injury(
    injury_data = injury_list,
    pitcher_game_log = pitcher_game_log
  )
  
  time_to_ucl_surgery <- pitchinj::get_time_to_injury(
    injury_data = ucl_surgery,
    pitcher_game_log = pitcher_game_log
  )
  
  time_to_injury <- time_to_injury_hits |>
    dplyr::rename(level_hits = level, severity_hits = severity, pitches_hits = pitches) |>
    dplyr::full_join(
      y = time_to_injury_list |>
        dplyr::rename(level_il = level, severity_il = severity, pitches_il = pitches),
      by = c("player_id", "year")
    ) |>
    dplyr::full_join(
      y = time_to_ucl_surgery |>
        dplyr::rename(level_ucl = level, severity_ucl = severity, pitches_ucl = pitches),
      by = c("player_id", "year")
    ) |>
    dplyr::mutate(
      # If severity/pitches are missing for HITS, this most often means that the injury was first
      # encountered in the season before the IL placement or UCL surgery. This could also happen if
      # the player was placed on the IL but days until clearance are below our threshold (7 days).
      severity_hits = dplyr::coalesce(severity_hits, "none"),
      pitches_hits = dplyr::coalesce(pitches_hits, 0),
      # After some digging, we learned that if severity/pitches are missing for IL, that's typically
      # because (a) the IL placement was unexpectedly missing from the data or (b) the TJ surgery
      # happened during rehab, so the corresponding IL placement was flagged as a continutation.
      # In either case, we want to use the UCL severity/pitches as the IL severity/pitches.
      severity_il = dplyr::coalesce(severity_il, severity_ucl),
      pitches_il = dplyr::coalesce(pitches_il, pitches_ucl),
      # If severity/pitches are missing for UCL, that's because the player missed the whole season
      # due to an IL placement which is not due to a UCL surgery in the current year.
      severity_ucl = dplyr::coalesce(severity_ucl, "none"),
      pitches_ucl = dplyr::coalesce(pitches_ucl, 0)
    ) |>
    dplyr::select(player_id, year, dplyr::starts_with("severity"), dplyr::starts_with("pitches"))
 
  injury_burden_list <- pitchinj::etl_injury_list(
    only_new_injury = FALSE,
    only_pitching_arm = TRUE,
    only_majors = TRUE
  ) |>
    dplyr::group_by(player_id, year) |>
    dplyr::summarize(burden = sum(season_days_missed), .groups = "drop")
  
  injury_burden <- injury_burden_list |>
    dplyr::rename(burden_il = burden)
 
  if (!is.null(pitch_tracking_data)) {

  pitch_tracking_prev_year <- pitch_tracking_data |>
    dplyr::mutate(
      # Scale usage so that each coefficient corresponds to a 10% increase in usage
      dplyr::across(dplyr::starts_with("usage_"), ~ .x * 10),
      dplyr::across(dplyr::starts_with("horz_"), ~ tidyr::replace_na(.x, 0)),
      dplyr::across(dplyr::starts_with("spin_"), ~ tidyr::replace_na(.x, 0)),
      year = year + 1,
      usage_SLSW = usage_SL + usage_SW,
      usage_CBKC = usage_CB + usage_KC,
      usage_CHSF = usage_CH + usage_SF,
      # We coalesce these with zero because they can only be NA if usage is zero.
      vert_FB_x_usage_FB = dplyr::coalesce(vert_FB * usage_FB / mean(usage_FB), 0),
      spin_SL_x_usage_SL = dplyr::coalesce(spin_SL * usage_SL / mean(usage_SL), 0),
      horz_gloveside = (
        usage_CT * horz_CT + usage_SL * horz_SL + usage_SW * horz_SW +
        usage_CB * horz_CB + usage_KC * horz_KC
      ) /
        (usage_CT + usage_SL + usage_SW + usage_CB + usage_KC + .Machine$double.xmin),
      horz_armside = (
        usage_CH * horz_CH + usage_SF * horz_SF
      ) /
        (usage_CH + usage_SF + .Machine$double.xmin),
      velo_primary_effort = velo_primary / velo_90_primary,
      spin_primary_effort = spin_primary / spin_90_primary
    )

  } else {
    # Create an empty table with the appropriate column names so that joins "work"
    pitch_tracking_prev_year <- tibble::tibble(player_id = NA, year = NA) |>
      dplyr::slice(c())
  }

  if (!is.null(biomechanics_data)) {
  
    biomechanics_prev_year <- biomechanics_data |>
      dplyr::mutate(year = year + 1) |>
      dplyr::select(player_id, year, arm_slot, dplyr::all_of(biomechanics_metrics))

  } else {
    # Create an empty table with the appropriate column names so that joins "work"
    biomechanics_prev_year <- tibble::tibble(player_id = NA, year = NA) |>
      dplyr::slice(c())
  }
  
  injury_history <- pitchinj::featurize_injury_history(
    injury_list = injury_list,
    season_summary = season_summary
  )
  
  data_year_over_year <- season_summary |>
    dplyr::full_join(time_to_injury, by = c("year", "player_id"), suffix = c("_ss", "_tt")) |>
    dplyr::full_join(injury_burden, by = c("year", "player_id")) |>
    dplyr::left_join(player, by = "player_id") |>   # for position
    dplyr::left_join(pitch_tracking_prev_year, by = c("player_id", "year")) |>
    dplyr::left_join(biomechanics_prev_year, by = c("player_id", "year")) |>
    dplyr::left_join(injury_history, by = c("player_id", "year")) |>
    dplyr::mutate(
      # For pitches, we prefer data coming from time_to_injury because that's aggregated from the
      # game logs prior to the first injury. However, the game logs miss some pitcher-seasons.
      pitches_hits = dplyr::coalesce(pitches_hits, pitches),
      pitches_il = dplyr::coalesce(pitches_il, pitches),
      pitches_ucl = dplyr::coalesce(pitches_ucl, pitches)
    ) |>
    # Get most recent role and level if currently NA
    dplyr::arrange(year) |>
    dplyr::group_by(player_id) |>
    tidyr::fill(role, level, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      severity_hits = factor(dplyr::coalesce(severity_hits, "none"), levels = c("none", "yes")),
      severity_il = factor(dplyr::coalesce(severity_il, "none"), levels = c("none", "short", "long")),
      severity_ucl = factor(dplyr::coalesce(severity_ucl, "none"), levels = c("none", "long")),
      burden_il = dplyr::coalesce(burden_il, 0),
      hand_left = as.integer(throw_hand == "L"),
      level_coarse = ifelse(level == "MLB", "majors", "minors"),
      level_minors = as.integer(level != "MLB"),
      role_reliever = as.integer(role == "reliever"),
      age = lubridate::interval(start = birth_date, end = glue::glue("{year}-07-01")) /
        lubridate::years(),
      past_injury = as.integer(dplyr::coalesce(past_injury, FALSE))
    ) |>
    dplyr::filter(
      primary_position == 1,
      batters_faced >= 1 | (severity_il != "none") | (severity_ucl != "none"),
      level %in% c("A+", "AA", "AAA", "MLB"),
      year > 2017
    )

  return(data_year_over_year)
}