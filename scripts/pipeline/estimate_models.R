
devtools::load_all("package/pitchinj")
dir.create("output/models", recursive = TRUE, showWarnings = FALSE)

logger::log_info("Loading data")

player <- data.table::fread("input/data/player.csv")
pitch_tracking_list <- pitchinj::etl_pitch_tracking()


logger::log_info("Wrangling data")

data_year_over_year <- pitchinj::wrangle_data_year_over_year(
  player = player,
  pitch_tracking_data = pitch_tracking_list$pitch_tracking
)

pitcher_subset <- pitchinj::subset_pitchers(data_year_over_year, verbose = TRUE)
scaled_features <- pitchinj::get_features(only_scaled = TRUE)

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

feature_subset_univariate <- pitchinj::get_features(
  type = "ballistics",
  model = "univariate"
)

coxph_univariate <- list()

for (outcome in c("hits", "ucl")) {

  coxph_univariate[[outcome]] <- list()

  pitchers <- pitcher_subset$full
  features <- feature_subset_univariate
  severity_col <- paste0("severity_", outcome)
  pitches_col <- paste0("pitches_", outcome)

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

    # Estimate univariate Cox PH regression models
    coxph_univariate[[outcome]][[feature]] <- data_feature |>
      pitchinj::standardize_data(model = "univariate") |>
      pitchinj::fit_injury_model(model = "coxph", features = feature)

    coxph_univariate[[outcome]][[feature]]$effect_unit <- data_feature |>
      dplyr::summarize(
        n = dplyr::n(),
        mean = signif(mean(!! rlang::sym(feature)), 2),
        sd = signif(sd(!! rlang::sym(feature)), 2),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        unit = ifelse(feature %in% scaled_features, signif(sd, 2), 1),
        variable = feature
      )
  }
}


# Estimate multivariate models ----

model_manifest <- tibble::tribble(
  ~subset, ~outcome,
  "full", "hits",
  "full", "ucl",
  "matched", "hits",
  "majors", "hits",
  "minors", "hits"
)

feature_subset_multivariate <- pitchinj::get_features(
  type = c("demographics", "ballistics"),
  model = "multivariate"
)

coxph_multivariate <- list()
coxph_multivariate_cv <- list()

for (subset in c("minors", "majors", "matched", "full")) {

  coxph_multivariate[[subset]] <- list()

  pitchers <- pitcher_subset[[subset]]

  effect_unit <- data_year_over_year |>
    dplyr::inner_join(pitchers, by = c("player_id", "year")) |>
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::all_of(feature_subset_multivariate),
        .fns = sd
      )
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "sd") |>
    dplyr::mutate(unit = ifelse(variable %in% scaled_features, signif(sd, 2), 1)) |>
    dplyr::select(variable, unit)

  data_subset <- data_year_over_year |>
    dplyr::inner_join(pitchers, by = c("player_id", "year")) |>
    pitchinj::standardize_data() |>
    # re-caculate interactions after standardization
    dplyr::mutate(
      # We coalesce these with zero because they can only be NA if usage is zero.
      vert_FB_x_usage_FB = dplyr::coalesce(vert_FB * usage_FB / mean(usage_FB), 0),
      spin_SL_x_usage_SL = dplyr::coalesce(spin_SL * usage_SL / mean(usage_SL), 0)
    )

  for (outcome in c("ucl", "hits")) {

    is_in_manifest <- model_manifest |>
      dplyr::filter(subset == !!subset, outcome == !!outcome) |>
      nrow() |>
      as.logical()

    if (is_in_manifest) {

      coxph_multivariate[[subset]][[outcome]] <- data_subset |>
        dplyr::mutate(
          severity = !!rlang::sym(glue::glue("severity_{outcome}")),
          pitches = !!rlang::sym(glue::glue("pitches_{outcome}"))
        ) |>
        pitchinj::fit_injury_model(model = "coxph", features = feature_subset_multivariate)

      coxph_multivariate[[subset]][[outcome]]$effect_unit <- effect_unit
    }
  }

  if (subset == "full") {
    pred_injury_starter <- pitchinj::predict_injury(
      object = coxph_multivariate$full$hits,
      newdata = data_subset |>
        dplyr::filter(level_minors == 0, role_reliever == 0),
      newtime = 2500
    ) |>
      dplyr::mutate(role = "starter", .before = 1)
  
    pred_injury_reliever <- pitchinj::predict_injury(
      object = coxph_multivariate$full$hits,
      newdata = data_subset |>
        dplyr::filter(level_minors == 0, role_reliever == 1),
      newtime = 1000
    ) |>
      dplyr::mutate(role = "reliever", .before = 1)

    pred_injury <- dplyr::bind_rows(pred_injury_starter, pred_injury_reliever)
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
    coxph_multivariate_cv[[subset]] <- list()
    prob_injury_cv <- NULL

    for (k in 1:num_folds) {

      coxph_multivariate_cv[[subset]][[k]] <- list()

      coxph_multivariate_cv[[subset]][[k]]$hits <- data_subset |>
        dplyr::filter(!player_id %in% fold[[k]]) |>
        dplyr::mutate(severity = severity_hits, pitches = pitches_hits) |>
        pitchinj::fit_injury_model(model = "coxph", features = feature_subset_multivariate)

      prob_injury_starter <- pitchinj::predict_injury(
        object = coxph_multivariate_cv[[subset]][[k]]$hits,
        newdata = data_subset |>
          dplyr::filter(player_id %in% fold[[k]]),
        newtime = 2500
      )$prob_inj

      prob_injury_reliever <- pitchinj::predict_injury(
        object = coxph_multivariate_cv[[subset]][[k]]$hits,
        newdata = data_subset |>
          dplyr::filter(player_id %in% fold[[k]]),
        newtime = 1000
      )$prob_inj

      prob_injury_cv <- data_subset |>
        dplyr::filter(player_id %in% fold[[k]]) |>
        dplyr::select(year, player_id, pitches = pitches_hits, role, level_coarse, severity = severity_hits) |>
        dplyr::mutate(
          prob_injury = ifelse(role == "starter", prob_injury_starter, prob_injury_reliever)
        ) |>
        dplyr::bind_rows(prob_injury_cv)
    }

    prob_injury_cv <- prob_injury_cv |>
      dplyr::mutate(
        quantile = floor((rank(prob_injury, ties.method = "first") - 1) / dplyr::n() * num_quantiles),
        .by = c(level_coarse, role)
      )

    extract_cumulative_incidence <- function(data, newtime) {

      fit <- pitchinj::fit_injury_model(data, features = "", strata = NULL)
      pred <- pitchinj::predict_injury(fit, newdata = data, newtime = newtime)
      return(pred)
    }

    cumulative_incidence_cv <- tibble::tibble()

    for (ell in c("majors")) {
      for (r in c("starter", "reliever")) {
        for (q in 1:num_quantiles - 1) {
          pred_subset <- prob_injury_cv |>
              dplyr::filter(level_coarse == ell, role == r, quantile == q)
          cumulative_incidence_cv <- extract_cumulative_incidence(
            data = pred_subset,
            newtime = ifelse(r == "starter", 2500, 1000)
          ) |>
            dplyr::mutate(
              level = ell,
              role = r,
              quantile = q,
              pred_prob_injury = mean(pred_subset$prob_injury),
              .before = 1
            ) |>
            dplyr::bind_rows(cumulative_incidence_cv)
        }
      }
    }
  }

}


# Write estimated models to file ----
logger::log_info("Saving results")

data.table::fwrite(pitch_tracking_list$pitch_reclass, file = "output/models/pitch_reclass.csv")
data.table::fwrite(pitch_tracking_list$vendor_translation, file = "output/models/vendor_translation.csv")
data.table::fwrite(data_year_over_year, file = "output/models/data_year_over_year.csv")
data.table::fwrite(pred_injury, file = "output/models/pred_injury.csv")
data.table::fwrite(cumulative_incidence_cv, file = "output/models/cumulative_incidence_cv.csv")
saveRDS(coxph_univariate, file = "output/models/coxph_univariate.rds")
saveRDS(coxph_multivariate$full$hits, file = "output/models/coxph_multivariate_hits_full.rds")
saveRDS(coxph_multivariate$full$ucl, file = "output/models/coxph_multivariate_ucl.rds")
saveRDS(coxph_multivariate$matched$hits, file = "output/models/coxph_multivariate_hits_matched.rds")
saveRDS(coxph_multivariate$majors$hits, file = "output/models/coxph_multivariate_hits_majors.rds")
saveRDS(coxph_multivariate$minors$hits, file = "output/models/coxph_multivariate_hits_minors.rds")

logger::log_info("Done")
