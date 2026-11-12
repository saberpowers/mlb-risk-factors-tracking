#' Default Feature Set for Injury Risk Modeling
#'
#' A tibble containing the default set of features used in pitcher injury risk models. Each feature
#' is annotated with its type (demographics, ballistics, or biomechanics), as well as preprocessing
#' flags for centering and scaling, and a human-readable display name.
#'
#' @format A tibble with 13 rows and 5 columns:
#' \describe{
#'   \item{variable}{Character. The variable name as it appears in the dataset.}
#'   \item{type}{Character. The category of the feature: `"demographics"`, `"ballistics"`, or `"biomechanics"`.}
#'   \item{center}{Logical. Whether the feature should be mean-centered.}
#'   \item{scale}{Logical. Whether the feature should be scaled (standardized).}
#'   \item{univariate}{Logical. Whether the feature should be included among the univariate models.}
#'   \item{multivariate}{Logical. Whether the feature should be inclued in the multivariate model.}
#'   \item{filter}{Character. Parseable as code specifying pitchers to be included for this feature.}
#'   \item{display}{Character. A user-friendly name for displaying the feature in tables or plots.}
#' }
#'
#' @export
default_features <- tibble::tribble(
  ~variable,                            ~type,          ~center,  ~scale, ~univariate,  ~multivariate,  ~biomechanics,  ~filter,                          ~display,
  "year",                               "demographics", TRUE,     FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "Year",
  "hand_left",                          "demographics", FALSE,    FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "Hand: Left (vs. Right)",
  "past_injury",                        "demographics", FALSE,    FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "Past Injury (0/1)",
  "age",                                "demographics", TRUE,     FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "Age (years)",
  "usage_FB",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "FB Usage",
  "usage_SI",                           "ballistics",   FALSE,    FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "SI Usage",
  "usage_CT",                           "ballistics",   FALSE,    FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "CT Usage",
  "usage_SL",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "SL Usage",
  "usage_SW",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "SW Usage",
  "usage_CB",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "CB Usage",
  "usage_KC",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "KC Usage",
  "usage_CH",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "CH Usage",
  "usage_SF",                           "ballistics",   FALSE,    FALSE,  TRUE,         FALSE,          FALSE,          "TRUE",                           "SF Usage",
  "usage_SLSW",                         "ballistics",   FALSE,    FALSE,  FALSE,        TRUE,           TRUE,           "TRUE",                           "SL+SW Usage",
  "usage_CBKC",                         "ballistics",   FALSE,    FALSE,  FALSE,        TRUE,           TRUE,           "TRUE",                           "CB+KC Usage",
  "usage_CHSF",                         "ballistics",   FALSE,    FALSE,  FALSE,        TRUE,           TRUE,           "TRUE",                           "CH+SF Usage",
  "velo_primary",                       "ballistics",   TRUE,     FALSE,  TRUE,         TRUE,           TRUE,           "TRUE",                           "Primary Velo",
  "spin_primary",                       "ballistics",   TRUE,     TRUE,   TRUE,         TRUE,           FALSE,          "TRUE",                           "Primary Spin Rate",
  "relx_primary",                       "ballistics",   TRUE,     TRUE,   TRUE,         TRUE,           FALSE,          "TRUE",                           "Lateral Release Location",
  "vert_FB",                            "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_FB > 0.1",                 "FB Vert Break",
  "spin_SL",                            "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_SL > 0.1",                 "SL Spin Rate",
  "spin_CB",                            "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_CB > 0.1",                 "CB Spin Rate",
  "vert_FB_x_usage_FB",                 "ballistics",   FALSE,    FALSE,  FALSE,        TRUE,           TRUE,           "TRUE",                           "FB Vert Break*",
  "spin_SL_x_usage_SL",                 "ballistics",   FALSE,    FALSE,  FALSE,        TRUE,           FALSE,          "TRUE",                           "SL Spin Rate*",
  "horz_gloveside",                     "ballistics",   TRUE,     TRUE,   TRUE,         TRUE,           FALSE,          "TRUE",                           "Glove-Side Break",
  "horz_armside",                       "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_CH + usage_SF > 0.1",      "Arm-Side Break",
  "stuff",                              "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "!is.na(stuff)",                  "Stuff+",
  "location",                           "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "!is.na(location)",               "Location+",
  "velo_sd_primary",                    "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "TRUE",                           "Primary Velo Std Dev",
  "velo_primary_effort",                "ballistics",   TRUE,     TRUE,   TRUE,         TRUE,           FALSE,          "TRUE",                           "Primary Velo Effort Ratio",
  "leverage_velo_diff",                 "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "leverage_velo_count >= 20",      "Primary Velo Leverage Diff",
  "spin_sd_primary",                    "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "TRUE",                           "Primary Spin Std Dev",
  "spin_primary_effort",                "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "TRUE",                           "Primary Spin Effort Ratio",
  "spin_sd_SL",                         "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_SL > 0.1",                 "SL Spin Std Dev",
  "spin_sd_CB",                         "ballistics",   TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "usage_CB > 0.1",                 "CB Spin Std Dev",
  "dom_elbow_var_inv_max",              "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Elbow Varus Torque Max",
  "dom_elbow_var_inv_mer",              "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          FALSE,          "!is.na(dom_elbow_var_inv_mer)",  "Elbow Varus Torque @MSER",
  "arm_slot",                           "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Arm Slot",
  "pitch_shoulder_horzabd_max",         "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Shoulder Horizontal Abduction Min",
  "lead_arm_proj_max",                  "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Glove Arm Momentum Max",
# We are currently excluding torso_vert_ang_align because of its very heavy tails (kurtosis > 5)
#  "torso_vert_ang_align",               "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Torso Momementum Alignment",
  "torso_proj_max",                     "biomechanics", TRUE,     TRUE,   TRUE,         FALSE,          TRUE,           "TRUE",                           "Torso Momentum Max",
)

#' Retrieve Feature Names Based on Type and Preprocessing Flags
#'
#' Returns a character vector of feature names from `default_features`, filtered
#' by feature type and optional flags for whether the features are centered or scaled.
#'
#' @param type Character vector indicating the types of features to include.
#'   Must be one or more of `"demographics"`, `"ballistics"`, or `"biomechanics"`.
#' @param model Character vector indicating which model for which to filter variables.
#'   Must be one of `"TRUE"`, `"univariate"`, `"multivariate"` or `"biomechanics"`.
#'   Default, `"TRUE"`, does not apply any filtering by model.
#' @param only_centered Logical. If `TRUE`, only return features that are centered.
#' @param only_scaled Logical. If `TRUE`, only return features that are scaled.
#'
#' @return A character vector of feature names satisfying the specified criteria.
#'
#' @examples
#' get_features("demographics")
#' get_features(c("ballistics", "biomechanics"), only_centered = TRUE)
#'
#' @export
get_features <- function(type = c("demographics", "ballistics", "biomechanics"),
                         model = c("TRUE", "univariate", "multivariate", "biomechanics"),
                         only_centered = FALSE,
                         only_scaled = FALSE) {

  type <- match.arg(type, several.ok = TRUE)
  model <- match.arg(model)

  features <- default_features |>
    dplyr::filter(
      type %in% !!type, center | !only_centered, scale | !only_scaled,
      eval(parse(text = model))    # only include features for the specified model
    ) |>
    dplyr::pull(variable)

  return(features)
}

#' Get Reboot Metrics for Lasso Model
#'
#' Returns the set of Reboot motion capture features used in the lasso model for pitcher biomechanics.
#' 
#' @param file_path character string specifying path to reboot_glossary.csv
#'
#' @return A vector of Reboot metric names used as predictors in the lasso model.
#' 
#' @export
get_reboot_metrics <- function(file_path = "docs/reboot_glossary.csv") {

  reboot_metrics <- data.table::fread(file_path) |>
    dplyr::filter(is_included)

  return(reboot_metrics)
}
