#' Clean pitch-level tracking data and summarize on the player-year level
#'
#' Cleans and standardizes pitch-level tracking data to produce a player-year
#' dataset summarizing pitch type characteristics. The function reads from
#' `"input/data/pitch_tracking_by_pitch.csv"`, cleans mislabeled pitch types using
#' tracking-based classifications, and aggregates release speed, spin, and break
#' metrics by pitch type.
#'
#' @return A data frame (`pitch_tracking_clean`) with one row per player-year
#' containing summarized pitch characteristics and usage rates.
#' @details Pitch types are cleaned separately for major and minor leagues to
#' ensure consistency in labeling based on tracking data rather than manual input.
#' @examples
#' pitch_tracking_clean <- etl_pitch_tracking_by_pitch()
#' @export
etl_pitch_tracking_by_pitch <- function() {

  pitch_tracking_by_pitch <- data.table::fread("input/data/pitch_tracking_by_pitch.csv")
  
  pitcher_fastball_velo <- pitch_tracking_by_pitch |>
    dplyr::filter(
      pitch_type %in% c("4-Seam Fastball", "Four-Seam", "Fastball", "Sinker", "2-Seam Fastball")
    ) |>
    dplyr::group_by(pitcher, year) |>
    dplyr::summarize(fastball_velo = median(p_release_speed), .groups = "drop")
  
  pitch_tracking_by_pitch_tidy <- pitch_tracking_by_pitch |>
    dplyr::left_join(pitcher_fastball_velo, by = c("pitcher", "year")) |>
    dplyr::mutate(
      pitch_type_old = dplyr::case_when(
        pitch_type %in% c("4-Seam Fastball", "Four-Seam", "Four-seam FB", "Fastball") ~ "FB",
        pitch_type %in% c("Slider") ~ "SL",
        pitch_type %in% c("Sinker", "2-Seam Fastball") ~ "SI",
        pitch_type %in% c("Changeup", "ChangeUp") ~ "CH",
        pitch_type %in% c("Curveball") ~ "CB",
        pitch_type %in% c("Cutter") ~ "CT",
        pitch_type %in% c("Sweeper") ~ "SW",
        pitch_type %in% c("Split-Finger", "Splitter") ~ "SF",
        pitch_type %in% c("Knuckle Curve") ~ "KC",
        pitch_type %in% c("Slurve") ~ "SV",
        pitch_type %in% c("", "Undefined") ~ "NA"
      )
    ) |>
    dplyr::filter(
      !is.na(pitch_type),
      p_release_speed > 60,
      abs(x_break_hand) < 36,
      abs(z_break_induced) < 36,
      # There are some massive outliers in x-release, so we'll trim the distribution
      x_release < quantile(x_release, probs = .999, na.rm = TRUE),
      x_release > quantile(x_release, probs = .001, na.rm = TRUE)
    ) |>
    tidyr::drop_na()
  
  rm(pitch_tracking_by_pitch)
  gc()
  
  pitch_tracking_by_pitch_majors <- pitch_tracking_by_pitch_tidy |>
    dplyr::filter(Lvl == "MLB") |>
    dplyr::mutate(level = "majors")
  
  pitch_tracking_by_pitch_minors <- pitch_tracking_by_pitch_tidy |>
    dplyr::filter(Lvl != "MLB") |>
    dplyr::mutate(level = "minors")

  rm(pitch_tracking_by_pitch_tidy)
  gc()
  
  pitch_tracking_by_pitch_majors <- pitch_tracking_by_pitch_majors |>
    dplyr::mutate(pitch_type_new = clean_pitch_type(pitch_tracking_by_pitch_majors))
  
  pitch_tracking_by_pitch_minors <- pitch_tracking_by_pitch_minors |>
    dplyr::mutate(pitch_type_new = clean_pitch_type(pitch_tracking_by_pitch_minors))

  metric_columns <- c(
    "p_release_speed", "p_release_spin_rate", "x_break_hand", "z_break_induced", "x_release"
  )

  vendor_translation <- pitch_tracking_by_pitch_minors |>
    dplyr::group_by(year, pitcher, pitch_type_new, vendor) |>
    dplyr::summarize(
      n = dplyr::n(),
      dplyr::across(.cols = metric_columns, .fns = mean),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 10) |>
    dplyr::select(-n) |>
    tidyr::pivot_longer(cols = dplyr::all_of(metric_columns), names_to = "metric") |>
    tidyr::pivot_wider(names_from = vendor) |>
    dplyr::filter(!is.na(TrackMan), !is.na(HawkEye)) |>
    dplyr::group_by(pitch_type_new, metric) |>
    dplyr::summarize(
      translation = mean(HawkEye - TrackMan),
      standard_error = sd(HawkEye - TrackMan) / sqrt(dplyr::n()),
      .groups = "drop"
    )
  
  pitch_tracking_by_pitch_minors_translated <- pitch_tracking_by_pitch_minors |>
    tidyr::pivot_longer(cols = metric_columns, names_to = "metric") |>
    dplyr::left_join(vendor_translation, by = c("pitch_type_new", "metric")) |>
    dplyr::mutate(value = value + ifelse(vendor == "TrackMan", translation, 0)) |>
    dplyr::select(-translation, -standard_error) |>
    tidyr::pivot_wider(names_from = metric)

  rm(pitch_tracking_by_pitch_minors)
  gc()

  pitch_by_pitch <- dplyr::bind_rows(
    pitch_tracking_by_pitch_majors,
    pitch_tracking_by_pitch_minors_translated
  ) |>
    dplyr::select(
      year, level, pitcher,
      p_release_speed, p_release_spin_rate, x_break_hand, z_break_induced, x_release, fastball_velo,
      pitch_type_old, pitch_type_new
    )
  
  rm(pitch_tracking_by_pitch_majors)
  rm(pitch_tracking_by_pitch_minors_translated)
  gc()
 
  pitcher_summary <- pitch_by_pitch |>
    dplyr::group_by(player_id = pitcher, year, pitch_type_new) |>
    dplyr::summarize(
      n = dplyr::n(),
      velo = mean(p_release_speed),
      velo_sd = sd(p_release_speed),
      velo_90 = quantile(p_release_speed, probs = 0.9),
      spin = mean(p_release_spin_rate),
      spin_sd = sd(p_release_spin_rate),
      spin_90 = quantile(p_release_spin_rate, probs = 0.9),
      relx = mean(x_release),
      horz = mean(x_break_hand),
      vert = mean(z_break_induced),
      .groups = "drop"
    ) |>
    dplyr::group_by(player_id, year) |>
    dplyr::mutate(usage = n / sum(n)) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(
      cols = c(
        "n", "usage", dplyr::starts_with("velo"), dplyr::starts_with("spin"),
        "relx", "horz", "vert"
      )
    ) |>
    dplyr::mutate(name = paste0(name, "_", pitch_type_new)) |>
    dplyr::select(player_id, year, name, value) |>
    tidyr::pivot_wider() |>
    dplyr::mutate(
      primary = dplyr::case_when(
        n_FB >= 20 & (velo_FB >= dplyr::coalesce(velo_SI, 0) | n_SI < 20) ~ "FB",
        n_SI >= 20 & (velo_SI >= dplyr::coalesce(velo_FB, 0) | n_FB < 20) ~ "SI"
      ),
      n_primary = ifelse(primary == "FB", n_FB, n_SI),
      velo_primary = ifelse(primary == "FB", velo_FB, velo_SI),
      velo_sd_primary = ifelse(primary == "FB", velo_sd_FB, velo_sd_SI),
      velo_90_primary = ifelse(primary == "FB", velo_90_FB, velo_90_SI),
      spin_primary = ifelse(primary == "FB", spin_FB, spin_SI),
      spin_sd_primary = ifelse(primary == "FB", spin_sd_FB, spin_sd_SI),
      spin_90_primary = ifelse(primary == "FB", spin_90_FB, spin_90_SI),
      relx_primary = ifelse(primary == "FB", relx_FB, relx_SI),
      dplyr::across(dplyr::starts_with("n_"), ~ tidyr::replace_na(.x, 0)),
      dplyr::across(dplyr::starts_with("usage_"), ~ tidyr::replace_na(.x, 0))
    )

  return(
    list(
      pitcher_summary = pitcher_summary,
      pitch_by_pitch = pitch_by_pitch,
      vendor_translation = vendor_translation
    )
  )
}
