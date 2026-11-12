#' Clean Pitch Type Labels Using QDA
#'
#' Reclassifies pitch types based on tracking data to correct for inconsistencies 
#' in labeling across sources. In some datasets, major league classifications may 
#' reflect the pitcher's label, while minor league classifications may reflect 
#' a stringer's judgment. This function standardizes classifications based on 
#' the physical characteristics of each pitch.
#'
#' @param data A data frame containing pitch-level information with the following columns:
#' \code{pitch_type_code}, \code{p_release_speed}, \code{p_release_spin_rate}, 
#' \code{x_break_hand}, \code{z_break_induced}, and \code{fastball_velo}.
#'
#' @return A character vector of predicted pitch types, corresponding to the 
#' cleaned classifications.
#'
#' @details
#' The function fits a quadratic discriminant analysis (QDA) model using 
#' \code{MASS::qda()} to predict \code{pitch_type_old} from release and 
#' movement variables. Pitches labeled as \code{"SV"} (slurve) are excluded 
#' from model training to force predictions into slider or curveball categories.
#'
#' @examples
#' \dontrun{
#' cleaned_types <- clean_pitch_type(pitch_data)
#' }
#'
clean_pitch_type <- function(data) {
  data <- tidyr::drop_na(data)
  fit_qda <- MASS::qda(
    formula = pitch_type_old ~
      p_release_speed + p_release_spin_rate + x_break_hand + z_break_induced + fastball_velo,
    data = data |>
      # We only want to allow predictions to be these 9 pitch types
      dplyr::filter(pitch_type_old %in% c("FB", "SL", "SI", "CH", "CB", "CT", "SW", "SF", "KC"))
  )
  pred_qda <- as.character(predict(fit_qda, newdata = data)$class)
  return(pred_qda)
}
