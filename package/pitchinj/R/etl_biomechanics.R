#' Extract, Transform, and Load Biomechanics Data
#'
#' Reads raw biomechanics data from a parquet file, filters to each pitcher's primary fastball type,
#' and summarizes key biomechanical variables by player and year.
#' 
#' @param player a table of player data containing columns `player_id` and `throw_hand`
#' @param outlier_threshold a scalar threshold for excluding outlier pitches. When calculating the
#'   player-year median for each metric, we will exclude pitches that fall below
#'   Q1 - outlier_threshold * IQR or above Q3 + outlier_threshold * IQR.
#' @param kurtosis_threshold a scalar threshold for excluding metric. If the kurtosis across
#'   player-year medians (after removing outliers and pitchers with negative arm slots) is above
#'   kurtosis_threshold, then we 
#'
#' @return A data frame with one row per pitcher-year, containing median values for selected
#'   biomechanical metrics:
#' \describe{
#'   \item{player_id}{Integer. Unique player identifier.}
#'   \item{year}{Integer. Year of the pitch.}
#'   \item{lead_arm_proj_max}{Numeric. Median lead arm momentum projection.}
#'   \item{pitch_shoulder_horz_range_full_min}{Numeric. Median shoulder horizontal abduction.}
#'   \item{torso_vert_ang_align}{Numeric. Median torso momentum alignment.}
#'   \item{torso_proj_max}{Numeric. Median torso momentum projection.}
#' }
#'
#' @details
#' The function identifies a player's primary fastball type (most frequently thrown among
#' `"FF"`, `"FT"`, and `"FC"`) for each year and includes only pitches matching that pitch type in
#' the summary. Biomechanical metrics are aggregated using the median to reduce sensitivity to
#' outliers.
#'
#' @examples
#' biomechanics <- etl_biomechanics()
#' head(biomechanics)
#'
#' @export
etl_biomechanics <- function(player, outlier_threshold = 5, kurtosis_threshold = 3) {

  pitcher_year_arm_slot <- data.table::fread("input/data/pitch_tracking_by_player_year.csv") |>
    dplyr::rename(arm_slot = `Arm Angle`) |>
    dplyr::filter(!is.na(arm_slot), lvl == "MLB") |>
    dplyr::select(player_id = pitcher, year, arm_slot)

  # We assume each pitcher-year only appears once with a non-missing arm slot
  # For now this is true, but it may not be true in the future.
  pitcher_year_arm_slot_repeated <- pitcher_year_arm_slot |>
    dplyr::count(player_id, year) |>
    dplyr::filter(n > 1) |>
    nrow()

  if (pitcher_year_arm_slot_repeated > 0) {
    stop("`pitcher_year_arm_slot` has repeated pitcher-years! This needs to be fixed.")
  }

  biomechanics_raw <- dplyr::bind_rows(
    arrow::read_parquet("input/data/biomechanics/2023.parquet"),
    arrow::read_parquet("input/data/biomechanics/2024.parquet"),
    arrow::read_parquet("input/data/biomechanics/2025.parquet")
  )

  primary_fastball <- biomechanics_raw |>
    dplyr::filter(mocap_pitch_type %in% c("FF", "FT", "FC")) |>
    dplyr::count(
      player_id = as.integer(org_player_id),
      year = lubridate::year(session_date),
      mocap_pitch_type
    ) |>
    dplyr::group_by(player_id, year) |>
    dplyr::arrange(-n) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(player_id, year, mocap_pitch_type)

  biomechanics_by_pitch <- biomechanics_raw |>
    dplyr::mutate(year = lubridate::year(session_date), player_id = as.integer(org_player_id)) |>
    dplyr::inner_join(primary_fastball, by = c("player_id", "year", "mocap_pitch_type")) |>
    dplyr::left_join(dplyr::select(player, player_id, throw_hand), by = c("player_id")) |>
    dplyr::mutate(
      # Lead Arm Side Angular Momementum
      lead_arm_side_max_rel = ifelse(throw_hand == "R", lead_arm_side_max, -lead_arm_side_min),
      lead_arm_side_min_rel = ifelse(throw_hand == "R", lead_arm_side_min, -lead_arm_side_max),
      lead_arm_side_max_percent_rel = ifelse(throw_hand == "R", lead_arm_side_max_percent, -lead_arm_side_min_percent),
      lead_arm_side_min_percent_rel = ifelse(throw_hand == "R", lead_arm_side_min_percent, -lead_arm_side_max_percent),
      # Lead Leg Side Angular Momementum
      lead_leg_side_max_rel = ifelse(throw_hand == "R", lead_leg_side_max, -lead_leg_side_min),
      lead_leg_side_min_rel = ifelse(throw_hand == "R", lead_leg_side_min, -lead_leg_side_max),
      lead_leg_side_max_percent_rel = ifelse(throw_hand == "R", lead_leg_side_max_percent, -lead_leg_side_min_percent),
      lead_leg_side_min_percent_rel = ifelse(throw_hand == "R", lead_leg_side_min_percent, -lead_leg_side_max_percent),
      # Lower Half Side Angular Momementum
      lower_half_side_max_rel = ifelse(throw_hand == "R", lower_half_side_max, -lower_half_side_min),
      lower_half_side_min_rel = ifelse(throw_hand == "R", lower_half_side_min, -lower_half_side_max),
      lower_half_side_max_percent_rel = ifelse(throw_hand == "R", lower_half_side_max_percent, -lower_half_side_min_percent),
      lower_half_side_min_percent_rel = ifelse(throw_hand == "R", lower_half_side_min_percent, -lower_half_side_max_percent),
      # Pitch Forearm Side Angular Momementum
      pitch_forearm_side_max_rel = ifelse(throw_hand == "R", pitch_forearm_side_max, -pitch_forearm_side_min),
      pitch_forearm_side_min_rel = ifelse(throw_hand == "R", pitch_forearm_side_min, -pitch_forearm_side_max),
      pitch_forearm_side_max_percent_rel = ifelse(throw_hand == "R", pitch_forearm_side_max_percent, -pitch_forearm_side_min_percent),
      pitch_forearm_side_min_percent_rel = ifelse(throw_hand == "R", pitch_forearm_side_min_percent, -pitch_forearm_side_max_percent),
      # Pitch Hand Side Angular Momementum
      pitch_hand_side_max_rel = ifelse(throw_hand == "R", pitch_hand_side_max, -pitch_hand_side_min),
      pitch_hand_side_min_rel = ifelse(throw_hand == "R", pitch_hand_side_min, -pitch_hand_side_max),
      pitch_hand_side_max_percent_rel = ifelse(throw_hand == "R", pitch_hand_side_max_percent, -pitch_hand_side_min_percent),
      pitch_hand_side_min_percent_rel = ifelse(throw_hand == "R", pitch_hand_side_min_percent, -pitch_hand_side_max_percent),
      # Pitch Up Arm Side Angular Momementum
      pitch_up_arm_side_max_rel = ifelse(throw_hand == "R", pitch_up_arm_side_max, -pitch_up_arm_side_min),
      pitch_up_arm_side_min_rel = ifelse(throw_hand == "R", pitch_up_arm_side_min, -pitch_up_arm_side_max),
      pitch_up_arm_side_max_percent_rel = ifelse(throw_hand == "R", pitch_up_arm_side_max_percent, -pitch_up_arm_side_min_percent),
      pitch_up_arm_side_min_percent_rel = ifelse(throw_hand == "R", pitch_up_arm_side_min_percent, -pitch_up_arm_side_max_percent),
      # Rear Leg Side Angular Momementum
      rear_leg_side_max_rel = ifelse(throw_hand == "R", rear_leg_side_max, -rear_leg_side_min),
      rear_leg_side_min_rel = ifelse(throw_hand == "R", rear_leg_side_min, -rear_leg_side_max),
      rear_leg_side_max_percent_rel = ifelse(throw_hand == "R", rear_leg_side_max_percent, -rear_leg_side_min_percent),
      rear_leg_side_min_percent_rel = ifelse(throw_hand == "R", rear_leg_side_min_percent, -rear_leg_side_max_percent),
      # Torso Side Angular Momementum
      torso_side_max_rel = ifelse(throw_hand == "R", torso_side_max, -torso_side_min),
      torso_side_min_rel = ifelse(throw_hand == "R", torso_side_min, -torso_side_max),
      torso_side_max_percent_rel = ifelse(throw_hand == "R", torso_side_max_percent, -torso_side_min_percent),
      torso_side_min_percent_rel = ifelse(throw_hand == "R", torso_side_min_percent, -torso_side_max_percent),
      # Total Side Angular Momementum
      total_side_max_rel = ifelse(throw_hand == "R", total_side_max, -total_side_min),
      total_side_min_rel = ifelse(throw_hand == "R", total_side_min, -total_side_max),
      total_side_max_percent_rel = ifelse(throw_hand == "R", total_side_max_percent, -total_side_min_percent),
      total_side_min_percent_rel = ifelse(throw_hand == "R", total_side_min_percent, -total_side_max_percent),
      # Spine Rotation Range
      spine_rot_range_full_max_rel = ifelse(throw_hand == "R", spine_rot_range_full_max, -spine_rot_range_full_min),
      spine_rot_range_full_min_rel = ifelse(throw_hand == "R", spine_rot_range_full_min, -spine_rot_range_full_max),
      spine_rot_range_max_rel = ifelse(throw_hand == "R", spine_rot_range_max, -spine_rot_range_min),
      spine_rot_range_min_rel = ifelse(throw_hand == "R", spine_rot_range_min, -spine_rot_range_max),
      # Other Variables to Flip Directly
      move_direction_x_rel = ifelse(throw_hand == "R", move_direction_x, -move_direction_x),
      rear_ft_start_x_rel = ifelse(throw_hand == "R", rear_ft_start_x, -rear_ft_start_x),
      # Flip additional variables for improved interpretability
      rear_hip_er_range_max = -rear_hip_ir_range_min,
      rear_hip_er_range_full_max = -rear_hip_ir_range_full_min,
      hip_shoulder_separation_full_max_rel = -spine_rot_range_full_min_rel,
      hip_shoulder_separation_full_min_rel = -spine_rot_range_full_max_rel,
      hip_shoulder_separation_max_rel = -spine_rot_range_min_rel,
      hip_shoulder_separation_min_rel = -spine_rot_range_max_rel,
      lead_knee_ext_range_full_max = -lead_knee_flex_range_full_min,
      lead_knee_ext_range_max = -lead_knee_flex_range_min,
      rear_leg_gloveside_max_percent_rel = -rear_leg_side_min_percent_rel,
      pitch_shoulder_horzabd_max = -pitch_shoulder_horz_range_full_min,
      pitch_shoulder_horzabd_min = -dom_shoulder_flex_max,
      pitch_shoulder_horzabd_mer = -dom_shoulder_flex_mer,
      pitch_shoulder_horzabd_max_sequence = -pitch_shoulder_horz_range_min,
      pitch_shoulder_horzabd_min_sequence = -pitch_shoulder_horz_range_max,
      pitch_shoulder_horzabd_max_timing = pitch_shoulder_horz_range_min_norm_time,
      pitch_shoulder_horzabd_range_usage = pitch_shoulder_horz_range_norm
    )

  reboot_metrics <- get_reboot_metrics()$variable

  biomechanics_by_pitch_long <- biomechanics_by_pitch |>
    dplyr::select(org_movement_id, player_id, year, throw_hand, dplyr::all_of(reboot_metrics)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(reboot_metrics)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(outlier_index = calculate_outlier_index(value)) |>
    dplyr::ungroup()
  
  biomechanics_by_pitcher_long <- biomechanics_by_pitch_long |>
    dplyr::group_by(player_id, year, throw_hand, name) |>
    dplyr::summarize(
      n = sum(outlier_index < outlier_threshold),
      value = median(value[outlier_index < outlier_threshold], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(n >= 15) |>
    dplyr::select(player_id, year, throw_hand, name, value) |>
    dplyr::inner_join(pitcher_year_arm_slot, by = c("player_id", "year")) |>
    dplyr::filter(arm_slot > 0)

  metric_outlier <- biomechanics_by_pitch_long |>
    dplyr::group_by(name) |>
    dplyr::summarize(outlier = mean(outlier_index >= outlier_threshold, na.rm = TRUE))

  metric_kurtosis <- biomechanics_by_pitcher_long |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      kurtosis = e1071::kurtosis(value),
      threshold = kurtosis_threshold
    )
  
  biomechanics <- biomechanics_by_pitcher_long |>
    dplyr::left_join(metric_kurtosis, by = "name") |>
    # We simply drop any variable that is too heavy-tailed to avoid extreme high-leverage outliers
    dplyr::filter(kurtosis < threshold) |>
    dplyr::select(player_id, year, throw_hand, arm_slot, name, value) |>
    tidyr::pivot_wider()

  return(
    list(
      biomechanics = biomechanics,
      metric_outlier = metric_outlier,
      metric_kurtosis = metric_kurtosis
    )
  )
}
