#' Extract, transform and load player-year summary pitch tracking data
#' 
#' This function reads pitch tracking data from file and converts it into one row per
#' pitcher-year-pitch_type.
#' 
#' @return a table of pitch count, usage, velo and spin rate by player_id, year and pitch_type
#' 
#' @export
etl_pitch_tracking_by_player_year <- function() {

  pitch_types <- c(
    "FourSeam", "SI", "CT", "SL", "SW", "CU", "KCU", "CH", "SPL", "PrimaryFB", "UndefinedFB"
  )

  pitch_tracking_raw <- data.table::fread("input/data/pitch_tracking_by_player_year.csv")

  # These are the columns we observe once per pitcher-season, not separately for each pitch type
  summary_columns <- c("leverage_velo_count", "leverage_velo_diff", "stuff", "location")

  pitch_tracking_by_pitch_type <- pitch_tracking_raw |>
    dplyr::rename(
      level = lvl,
      player_id = pitcher,
      pitches = TotalPitches,
      primary_fb = PrimaryFB_Type,
      PrimaryFB = PrimaryFB_Count,
      leverage_velo_count = High_Leverage_PrimaryFB_Count
    ) |>
    # Aggregate leverage velo diff across levels within pitcher-season
    dplyr::group_by(player_id, year) |>
    dplyr::mutate(
      leverage_velo_diff = High_Leverage_PrimaryFB_Velo - Non_High_Leverage_PrimaryFB_Velo,
      leverage_velo_diff = ifelse(
        test = sum(leverage_velo_count, na.rm = TRUE) >= 20,
        yes = weighted.mean(leverage_velo_diff, w = leverage_velo_count, na.rm = TRUE),
        no = 0
      ),
      leverage_velo_count = sum(leverage_velo_count, na.rm = TRUE),
      stuff = weighted.mean(`Stuff+`[!is.na(`Stuff+`)], w = pitches[!is.na(`Stuff+`)]),
      location = weighted.mean(`Location+`[!is.na(`Stuff+`)], w = pitches[!is.na(`Location+`)])
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = dplyr::starts_with(pitch_types)) |>
    dplyr::mutate(
      underscore = stringr::str_locate(name, "_")[, 1],
      pitch_type = substring(name, 1, dplyr::coalesce(underscore - 1, nchar(name))),
      name = dplyr::coalesce(tolower(substring(name, underscore + 1, nchar(name))), "count")
    ) |>
    dplyr::select(
      player_id, year, level, vendor, pitches, dplyr::all_of(summary_columns),
      pitch_type, name, value
    ) |>
    tidyr::pivot_wider() |>
    dplyr::group_by(player_id, year, dplyr::across(dplyr::all_of(summary_columns)), pitch_type) |>
    dplyr::summarize(
      n = sum(count, na.rm = TRUE),
      usage = weighted.mean(usage, w = pitches),
      velo = weighted.mean(avgvelo, w = count, na.rm = TRUE),
      velo_sd = weighted.mean(sdvelo, w = count, na.rm = TRUE),
      velo_90 = weighted.mean(`90velo`, w = count, na.rm = TRUE),
      spin = weighted.mean(avgsr, w = count, na.rm = TRUE),
      spin_sd = weighted.mean(sdspin, w = count, na.rm = TRUE),
      spin_90 = weighted.mean(`90sr`, w = count, na.rm = TRUE),
      horz = weighted.mean(avgx, w = count, na.rm = TRUE),
      vert = weighted.mean(avgz, w = count, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n > 0)
    
  pitch_tracking <- pitch_tracking_by_pitch_type |>
    tidyr::pivot_longer(
      cols = c(
        "n", "usage", "velo", "velo_sd", "velo_90", "spin", "spin_sd", "spin_90", "horz", "vert"
      )
    ) |>
    dplyr::mutate(name = paste0(name, "_", pitch_type)) |>
    dplyr::select(player_id, year, dplyr::all_of(summary_columns), name, value) |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("n_"), ~ tidyr::replace_na(.x, 0)),
      dplyr::across(dplyr::starts_with("usage_"), ~ tidyr::replace_na(.x, 0))
    )

  return(pitch_tracking)
}
