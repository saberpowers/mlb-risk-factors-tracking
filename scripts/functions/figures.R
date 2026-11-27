
fig_pitch_class_settings <- function(fig_mode) {

  list(
    color = c(
      CT = sputil::color("red", mode = fig_mode),
      SL = sputil::color("yellow", mode = fig_mode),
      SV = sputil::color("gray", mode = fig_mode),
      CB = sputil::color("green", mode = fig_mode),
      SW = sputil::color("blue", mode = fig_mode)
    ),
    linetype = c(CT = "solid", SL = "solid", SV = "dashed", CB = "solid", SW = "solid")
  )
}

fig_pitch_class <- function(data,
                            fig_mode = c("light", "dark"),
                            xlim = c(-5, 22),
                            ylim = c(-21, 15),
                            legend_label = "Pitch Type") {

  fig_mode <- match.arg(fig_mode)

  settings <- fig_pitch_class_settings(fig_mode = fig_mode)

  data |>
    dplyr::mutate(pitch_type = factor(pitch_type, levels = c("CT", "SL", "SV", "CB", "SW"))) |>
    dplyr::filter(!is.na(pitch_type), level == "majors") |>
    ggplot2::ggplot(
      ggplot2::aes(x = x_break_hand, y = z_break_induced, color = pitch_type, linetype = pitch_type)
    ) +
    ggplot2::stat_ellipse(linewidth = 1) +
    ggplot2::scale_color_manual(name = "Original Pitch Type", values = settings$color) +
    ggplot2::scale_linetype_manual(name = "Original Pitch Type", values = settings$linetype) +
    ggplot2::labs(
      x = "Glove-side Horizontal Break (inches)",
      y = "Induced Vertical Break (inches)"
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position = "top")
}

fig_pitch_class_old <- function(pitch_reclass, ...) {
  pitch_reclass |>
    dplyr::rename(pitch_type = pitch_type_old) |>
    fig_pitch_class(legend_label = "Original Pitch Type", ...)
}

fig_pitch_class_new <- function(pitch_reclass, ...) {
  pitch_reclass |>
    dplyr::rename(pitch_type = pitch_type_new) |>
    fig_pitch_class(legend_label = "Re-Classified Pitch Type", ...)
}




fig_prob_injury_density <- function(pred_injury, fig_mode = c("light", "dark")) {

  fig_mode <- match.arg(fig_mode)
  
  colors <- c(sputil::color("blue", fig_mode), sputil::color("orange", fig_mode))

  pred_injury |>
   dplyr::mutate(role = factor(stringr::str_to_title(role), levels = c("Starter", "Reliever"))) |>
   ggplot2::ggplot(
    ggplot2::aes(x = prob_inj, y = ggplot2::after_stat(density * width), fill = role)
   ) +
   ggplot2::geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
   ggplot2::scale_color_manual(name = "Role", values = colors) +
   ggplot2::scale_fill_manual(name = "Role", values = colors) +
   ggplot2::scale_x_continuous(
     name = "Injury Probability",
     breaks = c(0.2, 0.3, 0.4, 0.5),
     labels = c("20%", "30%", "40%", "50%")
   ) +
   ggplot2::labs(y = "") +
   sputil::theme_sleek(mode = fig_mode) +
   sputil::remove_axes(remove_x = FALSE) +
   ggplot2::theme(legend.position.inside = c(0.8, 0.8))
}




fig_calibration <- function(cumulative_incidence_cv, fig_mode = c("light", "dark")) {

  fig_mode <- match.arg(fig_mode)

  breaks <- seq(from = 0.15, to = 0.5, by = 0.05)
  colors <- c(sputil::color("blue", fig_mode), sputil::color("orange", fig_mode))

  cumulative_incidence_cv |>
    dplyr::mutate(
      role = factor(stringr::str_to_title(role), levels = c("Starter", "Reliever"))
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = pred_prob_injury,
        y = prob_inj,
        ymin = prob_inj_lower,
        ymax = prob_inj_upper,
        color = role
      )
    ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      linewidth = 0.2
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(alpha = 0.2) +
    ggplot2::scale_color_manual(name = "Role", values = colors) +
    ggplot2::scale_x_continuous(
      name = "Injury Probability",
      breaks = breaks,
      labels = glue::glue("{100 * breaks}%")
    ) +
    ggplot2::scale_y_continuous(
      name = "Injury Probability",
      breaks = breaks,
      labels = glue::glue("{100 * breaks}%")
    ) +
    ggplot2::coord_cartesian(xlim = c(0.15, 0.5), ylim = c(0.15, 0.5)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.9, 0.1))
}

