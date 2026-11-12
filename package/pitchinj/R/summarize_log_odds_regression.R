#' Summarize Log-Odds Regression Output on the Effect Scale
#'
#' Converts log-odds regression model output into a table summarizing effect sizes and confidence
#' intervals on the relative (percent change) scale.
#'
#' @param fit A fitted model object. Supported types include objects of class
#'   \code{"coxph"}, \code{"clm"}, \code{"glm"}, \code{"brmsfit"}, and \code{"cv.glmnet"}.
#' @param variable_labels An optional named character vector or \code{NA}. If provided, used to
#'   rename variables for display. Defaults to \code{NA}.
#' @param format_pretty Logical. If \code{TRUE}, returns formatted output for LaTeX with percentage
#'   signs and signs (+/--). Defaults to \code{FALSE}.
#' @param confidence_interval_coverage A numeric value between 0 and 1 specifying the confidence
#'   level. Defaults to \code{0.95}.
#' @param bold_p_value_threshold A numeric threshold below which p-values will be printed in bold
#'   (if format_pretty is `TRUE`, otherwise ignored).
#' @param type_p_value A character vector of feature types for which to include p-values.
#'
#' @return A tibble with either raw numeric summaries or prettified text versions, depending on the
#'   \code{format_pretty} argument. Includes the estimated effect, lower and upper bounds of the
#'   confidence interval, and (optionally) formatted text intervals.
#'
#' @examples
#' \dontrun{
#' fit <- glm(injury ~ age + velocity, data = my_data, family = binomial())
#' summarize_log_odds_regression(fit)
#' }
#'
#' @export
summarize_log_odds_regression <- function(fit,
                                          grouped_features = c("usage_SI", "usage_CT", "usage_SLSW", "usage_CBKC", "usage_CHSF"),
                                          variable_labels = NA,
                                          format_pretty = FALSE,
                                          confidence_interval_coverage = 0.95,
                                          bold_p_value_threshold = 0.05,
                                          type_p_value = c("demographics", "ballistics", "biomechanics")) {

  p_value_column <- "Pr(>|z|)"
  wald_p_value <- tibble::tibble(variable = NA_character_, p_value_wald = NA_real_)
  if ("coxph" %in% class(fit)) {
    estimate_column <- "coef"
    standard_error_column <- "se(coef)"
    coef_matrix <- survival:::summary.coxph(fit)$coefficients
  } else if (any(c("clm", "glm") %in% class(fit))) {
    estimate_column <- "Estimate"
    standard_error_column <- "Std. Error"
    coef_matrix <- summary(fit)$coefficients
  } else if ("brmsfit" %in% class(fit)) {
    estimate_column <- "Estimate"
    standard_error_column <- "Est.Error"
    coef_matrix <- summary(fit)$fixed
  } else if ("cv.glmnet" %in% class(fit)) {
    estimate_column <- "estimate"
    standard_error_column <- "standard_error"
    coef_matrix <- glmnet:::coef.cv.glmnet(fit, s = "lambda.min") |>
      as.matrix() |>
      cbind(NA) |>  # N/A column for standard error
      cbind(NA)     # N/A column for p-value
    colnames(coef_matrix) <- c(estimate_column, standard_error_column, p_value_column)
    coef_matrix[coef_matrix == 0] <- NA   # don't print coefficients that are precisely zero
  }

  z_score <- -qnorm((1 - confidence_interval_coverage) / 2)

  if ("coxph" %in% class(fit) & !is.null(grouped_features)) {
    b <- coef(fit)[grouped_features]
    V <- vcov(fit)[grouped_features, grouped_features]
    wald_stat <- drop(crossprod(b, solve(V, b)))
    df <- length(grouped_features)

    wald_p_value <- tibble::tibble(variable = grouped_features)
    wald_p_value$p_value_wald <- NA
    wald_p_value$p_value_wald[1] <- pchisq(wald_stat, df = df, lower.tail = FALSE)
  } 

  coef_table <- coef_matrix |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::filter(!variable %in% c("(Intercept)", "Intercept", names(fit$alpha))) |>  # remove intercepts
    dplyr::left_join(wald_p_value, by = "variable") |>
    dplyr::left_join(default_features, by = "variable") |>
    dplyr::mutate(
      p_value = dplyr::case_when(
        !type %in% type_p_value ~ NA_real_,
        variable %in% grouped_features ~ p_value_wald,
        TRUE ~ !!rlang::sym(p_value_column)
      ),
      variable = dplyr::coalesce(display, variable),
      estimate = !!rlang::sym(estimate_column),
      standard_error = !!rlang::sym(standard_error_column),
      effect = exp(estimate) - 1,
      lower_bound = exp(estimate - z_score * standard_error) - 1,
      upper_bound = exp(estimate + z_score * standard_error) - 1
    ) |>
    dplyr::select(variable, effect, lower_bound, upper_bound, p_value)

  # Benjamini-Hochberg multiple testing correction
  
  p_value_vec <- coef_table |>
    dplyr::filter(!is.na(p_value)) |>
    dplyr::arrange(p_value) |>
    dplyr::pull(p_value)

  threshold_vec <- bold_p_value_threshold / length(p_value_vec) * 1:length(p_value_vec)

  bold_p_value_treshold_adjusted <- max(threshold_vec[p_value_vec < threshold_vec])  

  coef_table <- coef_table |>
    dplyr::mutate(significant = p_value < bold_p_value_treshold_adjusted)

  if (format_pretty) {

    coef_table_pretty <- coef_table |>
      dplyr::mutate(
        dplyr::across(
          .cols = c("effect", "lower_bound", "upper_bound"),
          .fns = \(x) {
            x_pct <- format_percentage(x)
            dplyr::case_when(
              is.na(x) ~ NA_character_,
              x > 0 ~ paste0("+", x_pct),
              x < 0 ~ paste0("-", x_pct),  # double dash in latex looks like a minus sign
              TRUE ~ x_pct
            )
          }
        ),
        interval = dplyr::case_when(
          is.na(lower_bound) | is.na(upper_bound) ~ NA_character_,
          TRUE ~ glue::glue("({lower_bound}, {upper_bound})")
        ),
        p_value_bold = p_value < bold_p_value_threshold,
        p_value_string = substring(sprintf("%.3f", p_value), 2),
        p_value = ifelse(
          test = p_value < bold_p_value_threshold,
          yes = paste0("{\\bf", p_value_string, "}"),
          no = p_value_string
        )
      ) |>
      dplyr::select(variable, effect, interval, p_value)

    return(coef_table_pretty)
  }

  return(coef_table)
}
