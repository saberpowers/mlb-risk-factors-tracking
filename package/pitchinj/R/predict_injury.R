#' Predict Injury Probability from a Cox Model
#'
#' Calculates the predicted probability of an injury occurring within specific time 
#' limits using a fitted Cox Proportional Hazards model. This is calculated as 
#' the cumulative incidence: \eqn{1 - S(t)}, where \eqn{S(t)} is the survival probability.
#'
#' @param object A fitted \code{coxph} object from the \code{survival} package.
#' @param newdata A data frame containing the covariates used to fit the original 
#'   model. Each row represents an observation for which you want to predict injury.
#' @param newtime A numeric vector of time points for which predictions are desired. 
#'   The units (e.g., days, hours) must match the units of time used when fitting 
#'   the original model.
#'
#' @details 
#' The function finds the closest available time point in the survival curve to 
#' each value provided in \code{newtime} using \code{which.min(abs())}.
#'
#' @return A \code{\link[tibble]{tibble}} with:
#' \itemize{
#'   \item One row for every row in \code{newdata}.
#'   \item One column for every element in the \code{newtime} vector, prefixed 
#'   with "prob_inj_" followed by the closest matched time point.
#' }
#' Each cell contains the predicted probability of injury for that observation 
#' within the specified time limit.
#'
#' @importFrom survival survfit
#' @importFrom tibble tibble
#' @importFrom dplyr select starts_with
#' @export
predict_injury <- function(object, newdata, newtime) {

  if (length(newtime) > 1) {
    stop("newtime must be a single time, ")
  }

  fit <- survival::survfit(object, newdata = newdata)

  # survival::survfit is a cursed function. The output format depends on model properties.
  if (is.null(fit$strata)) {
    if (is.matrix(fit$surv)) {
      # Handle typical case
      pred_all <- tibble::tibble(
        index = rep(as.integer(colnames(fit$surv)), each = nrow(fit$surv)),
        time = rep(fit$time, times = ncol(fit$surv)),
        surv = c(fit$surv),
        lower = c(fit$lower),
        upper = c(fit$upper)
      )
    } else {
      # Handle case where all observations have the same prediction (e.g., no covariates)
      pred_all <- tibble::tibble(
        index = 1,
        time = fit$time,
        surv = fit$surv,
        lower = fit$lower,
        upper = fit$upper
      )
    }
  } else {
    # Handle models with stratification variables
    pred_all <- tibble::tibble(
      index = rep(as.integer(names(fit$strata)), fit$strata),
      time = fit$time,
      surv = fit$surv,
      lower = fit$lower,
      upper = fit$upper
    )
  }
  
  pred <- pred_all |>
    dplyr::mutate(
      prob_inj = 1 - surv,
      prob_inj_lower = 1 - upper,
      prob_inj_upper = 1 - lower
    ) |>
    dplyr::filter(time <= newtime) |>
    dplyr::group_by(index) |>
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(index, time, prob_inj, prob_inj_lower, prob_inj_upper)

  return(pred)
}
