
tab_hits_multivariate <- function(multivariate_coxph) {

  multivariate_coxph$full$hits |>
    pitchinj::summarize_log_odds_regression(
      type_p_value = "ballistics",
      effect_unit = multivariate_coxph$full$effect_unit
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = c("effect", "lower_bound", "upper_bound"),
        .fns = \(x) {
          x_pct <- format_percentage(x)
          dplyr::case_when(
            is.na(x) ~ NA_character_,
            x > 0 ~ paste0("+", x_pct),
            TRUE ~ x_pct
          )
        }
      ),
      interval = dplyr::case_when(
        is.na(lower_bound) | is.na(upper_bound) ~ NA_character_,
        TRUE ~ glue::glue("({lower_bound}, {upper_bound})")
      ),
      p_value = dplyr::case_when(!is.na(p_value) ~ substring(sprintf("%.3f", p_value), 2))
    ) |>
    dplyr::select(variable, unit, effect, interval, p_value, significant) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      variable = "Feature",
      unit = "Effect Unit",
      effect = "Effect",
      interval = "95% Conf. Int.",
      p_value = "p"
    ) |>
    flextable::align(j = ~ unit + effect + interval + p_value, align = "center", part = "all") |>
    flextable::hline(c(4, 9)) |>
    flextable::bold(i = ~ dplyr::coalesce(significant, FALSE), j = ~ p_value) |>
    flextable::delete_columns("significant") |>
    flextable::set_table_properties(layout = "autofit")
}
