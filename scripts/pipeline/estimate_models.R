
devtools::load_all("package/pitchinj")

logger::log_info("Loading data")

player <- data.table::fread("input/data/player.csv")
pitch_tracking_list <- pitchinj::etl_pitch_tracking()


logger::log_info("Wrangling data")

data_year_over_year <- pitchinj::wrangle_data_year_over_year(
  player = player,
  pitch_tracking_data = pitch_tracking_list$pitch_tracking
)

pitcher_subset <- pitchinj::subset_pitchers(data_year_over_year)

# What is the baseline rate of IL placement and UCL surgery in the majors cohort?
data_year_over_year |>
  dplyr::inner_join(pitcher_subset$majors, by = c("year", "player_id")) |>
  dplyr::summarize(
    n = dplyr::n(),
    pitches_il = mean(pitches_il),
    rate_il = mean(severity_il != "none"),
    pitches_ucl = mean(pitches_ucl),
    rate_ucl = mean(severity_ucl != "none")
  )

# What is the baseline rate of IL placement and UCL surgery in the minors cohort?
data_year_over_year |>
  dplyr::inner_join(pitcher_subset$minors, by = c("year", "player_id")) |>
  dplyr::summarize(
    n = dplyr::n(),
    pitches_il = mean(pitches_il),
    rate_il = mean(severity_il != "none"),
    pitches_ucl = mean(pitches_ucl),
    rate_ucl = mean(severity_ucl != "none")
  )


logger::log_info("Estimating models")

# Report group differences and univariate Cox PH regression coefficients ----

feature_subset_univariate <- list(
  full = pitchinj::get_features(type = c("demographics", "ballistics"), model = "univariate"),
  majors = pitchinj::get_features(type = c("demographics", "ballistics"), model = "univariate"),
  minors = pitchinj::get_features(type = c("demographics", "ballistics"), model = "univariate")
)

group_difference <- list()
univariate_coxph <- list()

for (subset in c("full", "majors", "minors")) {

  group_difference[[subset]] <- list()
  univariate_coxph[[subset]] <- list()

  for (outcome in c("hits", "il", "ucl")) {

    group_difference[[subset]][[outcome]] <- list()
    univariate_coxph[[subset]][[outcome]] <- list()

    pitchers <- pitcher_subset[[subset]]
    features <- feature_subset_univariate[[subset]]
    severity_col <- paste0("severity_", outcome)
    pitches_col <- paste0("pitches_", outcome)

    # Manually remove Stuff+ and Location+ as features for Minors 2022--2025 cohort because it is
    # NA for the vast majority of pitchers. TODO: Clean this up by generalizing
    # pitchinj::get_features() to allow different features between majors and minors cohorts.
    if (subset == "minors") {
      features <- setdiff(features, c("stuff", "location"))
    }

    for (feature in features) {

      filter <- pitchinj::default_features |>
        dplyr::filter(variable == feature) |>
        dplyr::pull(filter)

      data_feature <- data_year_over_year |>
        dplyr::filter(eval(parse(text = filter))) |>
        dplyr::inner_join(pitchers, by = c("player_id", "year")) |>
        dplyr::mutate(
          severity = !!rlang::sym(severity_col),
          pitches = !!rlang::sym(pitches_col),
        )

      # Report averages in injured/non-injured groups
      group_difference[[subset]][[outcome]][[feature]] <- data_feature |>
        pitchinj::report_group_mean_difference(feature) |>
          tibble::add_column(n = nrow(data_feature), .after = "variable")

      # Estimate univariate Cox PH regression models
      univariate_coxph[[subset]][[outcome]][[feature]] <- data_feature |>
        pitchinj::standardize_data(model = "univariate") |>
        pitchinj::fit_injury_model(model = "coxph", features = feature, strata = NULL)
    }
  }
}


# Estimate multivariate models ----

feature_subset_multivariate <- list(
  full = pitchinj::get_features(type = c("demographics", "ballistics"), model = "multivariate"),
  majors = pitchinj::get_features(type = c("demographics", "ballistics"), model = "multivariate"),
  minors = pitchinj::get_features(type = c("demographics", "ballistics"), model = "multivariate")
)

multivariate_coxph <- list()
multivariate_coxph_cv <- list()
prob_injury_cv <- list()
cumulative_incidence_cv <- list()

for (subset in c("majors", "minors", "full")) {

  multivariate_coxph[[subset]] <- list()

  pitchers <- pitcher_subset[[subset]]
  features <- feature_subset_multivariate[[subset]]

  data_subset <- data_year_over_year |>
    dplyr::inner_join(pitchers, by = c("player_id", "year")) |>
    pitchinj::standardize_data() |>
    # re-caculate interactions after standardization
    dplyr::mutate(
      # We coalesce these with zero because they can only be NA if usage is zero.
      vert_FB_x_usage_FB = dplyr::coalesce(vert_FB * usage_FB / mean(usage_FB), 0),
      spin_SL_x_usage_SL = dplyr::coalesce(spin_SL * usage_SL / mean(usage_SL), 0)
    )

  if (subset == "full") {
    data.table::fwrite(data_subset, file = "output/models/data_full_cohort.csv")
  }

  for (outcome in c("il", "ucl", "hits")) {
    multivariate_coxph[[subset]][[outcome]] <- data_subset |>
      dplyr::mutate(
        severity = !!rlang::sym(glue::glue("severity_{outcome}")),
        pitches = !!rlang::sym(glue::glue("pitches_{outcome}"))
      ) |>
      pitchinj::fit_injury_model(model = "coxph", features = features)
  }


  # out-of-sample validation
  if (subset == "full") {

    num_quantiles <- 10
    num_folds <- 10
    unique_pitchers <- unique(data_subset$player_id)
    set.seed(42)  # for reproducibility of CV fold assignment
    fold <- split(
      unique_pitchers,
      f = sample(rep(1:num_folds, length = length(unique_pitchers)))
    )
    multivariate_coxph_cv[[subset]] <- list()
    prob_injury_cv[[subset]] <- list()
    prob_injury_cv[[subset]]$hits <- NULL

    for (k in 1:num_folds) {

      multivariate_coxph_cv[[subset]][[k]] <- list()

      multivariate_coxph_cv[[subset]][[k]]$hits <- data_subset |>
        dplyr::filter(!player_id %in% fold[[k]]) |>
        dplyr::mutate(severity = severity_hits, pitches = pitches_hits) |>
        pitchinj::fit_injury_model(model = "coxph", features = features)

      prob_injury_starter <- pitchinj::predict_injury(
        object = multivariate_coxph_cv[[subset]][[k]]$hits,
        newdata = data_subset |>
          dplyr::filter(player_id %in% fold[[k]]),
        newtime = 2500
      )$prob_inj

      prob_injury_reliever <- pitchinj::predict_injury(
        object = multivariate_coxph_cv[[subset]][[k]]$hits,
        newdata = data_subset |>
          dplyr::filter(player_id %in% fold[[k]]),
        newtime = 1000
      )$prob_inj

      prob_injury_cv[[subset]]$hits <- data_subset |>
        dplyr::filter(player_id %in% fold[[k]]) |>
        dplyr::select(year, player_id, pitches = pitches_hits, role, level_coarse, severity = severity_hits) |>
        dplyr::mutate(
          prob_injury = ifelse(role == "starter", prob_injury_starter, prob_injury_reliever)
        ) |>
        dplyr::bind_rows(prob_injury_cv[[subset]]$hits)
    }

    prob_injury_cv[[subset]]$hits <- prob_injury_cv[[subset]]$hits |>
      dplyr::mutate(
        quantile = floor((rank(prob_injury, ties.method = "first") - 1) / dplyr::n() * num_quantiles),
        .by = role
      )

    extract_cumulative_incidence <- function(data, newtime) {
      fit <- pitchinj::fit_injury_model(data, features = "")
      pred <- pitchinj::predict_injury(fit, newdata = data, newtime = newtime)
      return(pred)
    }

    cumulative_incidence_cv[[subset]]$hits <- tibble::tibble()

    for (ell in c("majors", "minors")) {
      for (r in c("starter", "reliever")) {
        for (q in 1:num_quantiles - 1) {
          cumulative_incidence_cv[[subset]]$hits <- extract_cumulative_incidence(
            data = prob_injury_cv[[subset]]$hits |>
              dplyr::filter(level_coarse == ell, role == r, quantile == q),
            newtime = ifelse(r == "starter", 2500, 1000)
          ) |>
            dplyr::mutate(
              role = r,
              quantile = q,
              pred_prob_injury = prob_injury_cv[[subset]]$hits |>
                dplyr::filter(role == r, quantile == q) |>
                with(mean(prob_injury)),
              .before = 1
            ) |>
            dplyr::bind_rows(cumulative_incidence_cv[[subset]]$hits)
        }
      }
    }
  }

}


# Write estimated models to file ----
logger::log_info("Saving results")

dir.create("output/models", recursive = TRUE, showWarnings = FALSE)
data.table::fwrite(pitch_tracking_list$pitch_reclass, file = "output/models/pitch_reclass.csv")
data.table::fwrite(pitch_tracking_list$vendor_translation, file = "output/models/vendor_translation.csv")
data.table::fwrite(data_year_over_year, file = "output/models/data_year_over_year.csv")
saveRDS(group_difference, file = "output/models/group_difference.rds")
saveRDS(univariate_coxph, file = "output/models/univariate_coxph.rds")
saveRDS(multivariate_coxph, file = "output/models/multivariate_coxph.rds")
saveRDS(cumulative_incidence_cv, file = "output/models/cumulative_incidence_cv.rds")
saveRDS(lasso_coxph, file = "output/models/lasso_coxph.rds")
saveRDS(lasso_velo, file = "output/models/lasso_velo.rds")

logger::log_info("Done")
