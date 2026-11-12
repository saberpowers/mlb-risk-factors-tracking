#' Extract, Transform and Load Pitch-Tracking Data
#'
#' This function executes the ETL (Extract, Transform, Load) pipeline for pitch tracking data. 
#' It integrates high-level player-year metrics (such as velocity leverage and "stuff" scores) 
#' with cleaned, granular pitcher summaries and pitch-by-pitch data.
#'
#' @details 
#' The function performs an inner join between the data retrieved from 
#' \code{etl_pitch_tracking_by_player_year()} and the summary component of 
#' \code{etl_pitch_tracking_by_pitch()} using \code{player_id} and \code{year} as keys.
#'
#' @return A named \code{list} containing two elements:
#' \itemize{
#'   \item \code{pitch_tracking}: A data frame containing joined player metrics including 
#'   leverage velocity, stuff, and location scores.
#'   \item \code{pitch_reclass}: A data frame containing the raw pitch-by-pitch 
#'   reclassification data.
#' }
#' 
#' @export
#' @importFrom dplyr select inner_join
#'
#' @examples
#' \dontrun{
#'   pitch_data <- etl_pitch_tracking()
#'   head(pitch_data$pitch_tracking)
#' }
#' 
etl_pitch_tracking <- function() {
  
  pitch_tracking_dirty <- etl_pitch_tracking_by_player_year()
  pitch_tracking_clean <- etl_pitch_tracking_by_pitch()

  pitch_tracking <- pitch_tracking_dirty |>
    dplyr::select(player_id, year, leverage_velo_count, leverage_velo_diff, stuff, location) |>
    dplyr::inner_join(pitch_tracking_clean$pitcher_summary, by = c("player_id", "year"))

  return(
    list(
      pitch_tracking = pitch_tracking,
      pitch_reclass = pitch_tracking_clean$pitch_by_pitch,
      vendor_translation = pitch_tracking_clean$vendor_translation
    )
  )
}
