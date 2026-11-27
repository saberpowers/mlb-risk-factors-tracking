
tab_multivariate <- function(object) {

  object |>
    pitchinj::summarize_log_odds_regression(
      type_p_value = "ballistics",
      effect_unit = object$effect_unit,
      format_pretty = TRUE
    ) |>
    dplyr::mutate(
      unit = ifelse(grepl("Usage", variable), glue::glue("{10 * unit}%"), as.character(unit))
    ) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      variable = "Feature",
      unit = "Effect Unit",
      effect = "Effect",
      interval = "95% Conf. Int.",
      p_value = "p"
    ) |>
    flextable::align(j = ~ unit + effect + interval + p_value, align = "center", part = "all") |>
    flextable::hline(i = c(4, 9)) |>
    flextable::vline(j = 2) |>
    flextable::bold(i = ~ dplyr::coalesce(significant, FALSE), j = ~ p_value) |>
    flextable::delete_columns("significant") |>
    flextable::set_table_properties(layout = "autofit")
}




tab_univariate <- function(coxph_univariate) {

  univariate_table <- NULL
  univariate_features <- pitchinj::get_features(model = "univariate", type = "ballistics")
  for (feature in univariate_features) {
    univariate_row <- pitchinj::summarize_log_odds_regression(
      fit = coxph_univariate$hits[[feature]],
      effect_unit = coxph_univariate$hits[[feature]]$effect_unit,
      grouped_features = NULL,
      bold_p_value_threshold = 0,
      format_pretty = TRUE
    )
    univariate_table <- dplyr::bind_rows(univariate_table, univariate_row)
  }

  univariate_table |>
    dplyr::select(variable, n, mean, sd, unit, effect, interval) |>
    dplyr::mutate(
      mean = ifelse(grepl("Usage", variable), glue::glue("{10 * mean}%"), as.character(mean)),
      sd = ifelse(grepl("Usage", variable), glue::glue("{10 * sd}%"), as.character(sd)),
      unit = ifelse(grepl("Usage", variable), glue::glue("{10 * unit}%"), as.character(unit))
    ) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      variable = "Feature",
      n = "N",
      mean = "Mean",
      sd = "SD",
      unit = "Effect Unit",
      effect = "Effect",
      interval = "95% Conf. Int."
    ) |>
    flextable::align(j = ~ mean + sd + unit + effect + interval, align = "center", part = "all") |>
    flextable::hline(i = 9) |>
    flextable::vline(j = 5) |>
    flextable::set_table_properties(layout = "autofit")
}
