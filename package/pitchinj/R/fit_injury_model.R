#' Fit an Injury Risk Model
#'
#' Fits an injury prediction model using one of several modeling approaches:
#' Cox proportional hazards, ordinal logistic regression, or binary logistic regression.
#'
#' @param data A data frame containing player data, including injury status and predictor variables.
#' @param model A string specifying the model type. One of `"coxph"`, `"ordinal"`, `"logistic"`, `"lasso"` or `"lasso_velo"`.
#'              Defaults to `"coxph"`. Determines the type of response variable and model fit.
#' @param features A character vector of predictor variable names to be used in the model formula.
#'                 By default, includes features from all categories: `"demographics"`, `"ballistics"`, and `"biomechanics"`.
#' @param cluster variable on which to cluster (`"coxph"` only, defaults to "player_id")
#' @param strata variable on which to stratify (`"coxph"` only, defaults to "role")
#' @param ... additional arguments to the model fitting function
#'
#' @return A fitted model object of class `coxph`, `clm`, or `glm`, depending on the selected model type.
#'
#' @details
#' - `"coxph"` fits a survival model predicting time until injury.
#' - `"ordinal"` fits an ordinal logistic regression model for multi-level injury status.
#' - `"logistic"` fits a binary logistic regression model predicting whether any injury occurred.
#'
#' @examples
#' \dontrun{
#' data <- load_player_data()
#' model_fit <- fit_injury_model(data, model = "logistic")
#' summary(model_fit)
#' }
#'
#' @export
fit_injury_model <- function(data,
                             model = c("coxph", "ordinal", "logistic", "lasso", "lasso_velo"),
                             features = get_features(
                               c("demographics", "ballistics", "biomechanics")
                             ),
                             cluster = "player_id",
                             strata = c("level_coarse", "role"),
                             ...) {

  model <- match.arg(model)

  features_string <- paste(features, collapse = " + ")

  if (model == "coxph") {
    response_string <- "survival::Surv(time = pitches, event = severity != 'none')"
    if (!is.null(cluster)) {
      cluster_string <- glue::glue(" + cluster({cluster})")
    } else {
      cluster_string <- ""
    }
    if (!is.null(strata)) {
      strata_string <- glue::glue(" + strata({paste(strata, collapse = ', ')})")
      distinct_strata <- data |>
        dplyr::select(dplyr::all_of(strata)) |>
        dplyr::distinct() |>
        nrow()
      # if the strata variable is the same for all rows, there may as well be no strata variable
      if (distinct_strata == 1) {
        strata <- NULL
        strata_string <- ""
      }
    } else {
      strata_string <- ""
    }
    fit <- survival::coxph(
      formula = as.formula(
        glue::glue("{response_string} ~ {paste(features, collapse = '+')} {cluster_string} {strata_string}")
      ),
      data = data,
      ...
    )

  } else if (model == "ordinal") {
    response_string <- "severity"
    fit <- ordinal::clm(
      formula = as.formula(glue::glue("{response_string} ~ {paste(features, collapse = '+')}")),
      data = data,
      ...
    )

  } else if (model == "logistic") {
    response_string <- "(severity != 'none')"
    fit <- glm(
      formula = as.formula(glue::glue("{response_string} ~ {paste(features, collapse = '+')}")),
      data = data,
      family = binomial(),
      ...
    )

  } else if (model == "lasso") {
    x <- model.matrix(
      object = as.formula(glue::glue("~ 0 + {paste(features, collapse = '+')}")),
      data = data
    )
    y <- survival::Surv(
      time = pmax(data$pitches, 1),     # glmnet chokes on zero-valued survival times
      event = data$severity != 'none'
    )
    fit <- glmnet::cv.glmnet(x = x, y = y, family = "cox", ...)

  } else if (model == "lasso_velo") {
    features <- setdiff(features, "velo_primary")
    x <- model.matrix(
      object = as.formula(glue::glue("~ 0 + {paste(features, collapse = '+')}")),
      data = data
    )
    y <- data$velo_primary
    fit <- glmnet::cv.glmnet(x = x, y = y, ...)
  }

  return(fit)
}
