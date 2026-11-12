#' Featurize injury history
#'
#' This function takes in injury list data and season summary data to return a table indicating
#' which pitcher-seasons have any prior injury. This column is TRUE if the pitcher has had any
#' injury list appearance before the season in question and FALSE otherwise. We explored many other
#' possible featurizations of injury history, but this very naive definition was found to have
#' surprisingly strong predictive power relative to its simplicity. Note that season summary data
#' is not required to calculate this feature, but we still include it in the function in case we
#' want to use it in the future (for example, to calculate a feature which depends on the number
#' of seasons in which the pitcher appeared, which we tried to do with underwhelming results).
#' 
#' @param injury_list a table of injury list placements indexed by `player_id` and `year`,
#'   as returned by \code{etl_injury_list}
#' @param season_summary a table of pitcher season summaries indexed by `player_id` and `year`,
#'   with `batters_faced` as the primary column of interest
#' @param years_included integer, number of years to included in injury lookback (default 6)
#' 
#' @return a table indexed by `player_id` and `year` with the logical column `past_injury`
#'   indicating whether the player had an IL placement prior to the year in question
#' 
#' @export
#' 
featurize_injury_history <- function(injury_list, season_summary, years_included = 6) {

  injury_placement <- injury_list |>
    dplyr::mutate(
      duration = lubridate::interval(date_listed, date_return) %/%
        lubridate::days() |>
        dplyr::coalesce(`IL Days`)    # 3% of return dates are missing
    ) |>
    dplyr::distinct(player_id, year) |>
    dplyr::mutate(placement = TRUE) |>
    dplyr::full_join(
      y = season_summary |>
        dplyr::filter(batters_faced >= 27, year > 2017),
      by = c("player_id", "year")
    ) |>
    dplyr::mutate(appearance = TRUE, placement = dplyr::coalesce(placement, FALSE)) |>
    dplyr::select(player_id, year, appearance, placement)
  
  injury_history_wide <- injury_placement
  
  for (i in 1:years_included) {
    injury_history_wide <- injury_history_wide |>
      dplyr::full_join(
        y = injury_placement |>
          dplyr::mutate(year = year + i) |>
          dplyr::rename_with(.fn = \(x) paste0(x, "_", i), .cols = c("appearance", "placement")),
        by = c("player_id", "year")
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, FALSE)))
  }
  
  injury_history <- injury_history_wide |>
    dplyr::select(player_id, year, dplyr::matches("appearance_|placement_")) |>
    tidyr::pivot_longer(cols = dplyr::matches("appearance_|placement_")) |>
    dplyr::mutate(
      lag = as.integer(substring(name, nchar(name))),
      name = substring(name, 1, nchar(name) - 2)
    ) |>
    tidyr::pivot_wider() |>
    dplyr::group_by(player_id, year) |>
    dplyr::summarize(past_injury = any(placement), .groups = "drop")

  return(injury_history)
}
