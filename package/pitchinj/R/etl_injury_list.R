#' Extract, transform and load injured list data
#' 
#' This function reads all data from the injured list (IL) CSV and removes IL placements that
#' appear to be continuations of IL placements from the previous season. Only arm/elbow and shoulder
#' injuries and only the first IL placement of each pitcher season are included.
#' 
#' @param only_new_injury logical, should continuation IL placements be excluded? An IL placement
#'   is a continuation if the placement occurs before the beginning of the regular season and the
#'   pitcher was on the IL at the end of the previous season.
#' @param only_pitching_arm logical, should non-throwing-arm IL placements be excluded?
#' @param only_majors, should minor league IL placements be excluded?
#' 
#' @return a table of IL placements indexed by player ID and year
#' 
#' @export
etl_injury_list <- function(only_new_injury = TRUE,
                            only_pitching_arm = TRUE,
                            only_majors = FALSE) {

  injury_list_raw <- data.table::fread("input/data/injury/list.csv") |>
    dplyr::rename(
      player_id = `Player ID`,
      body_side = `Body Side`,
      body_part = `Body Part`,
      position = Position
    ) |>
    dplyr::mutate(
      date_listed = as.Date(`Date of Placement`, format = "%m/%d/%y"),
      date_return = as.Date(`Reinstated On`, format = "%m/%d/%y"),
      year = lubridate::year(date_listed),
      severity = ifelse(
        test = grepl("60-Day", `Placed To`) |
          (`Placed To` == "Full-Season") |
          !(`Transferred 60-Day` %in% c("", "-")) |   # "-" used before 2025; "" used in 2025
          (`IL Days` >= 60),
        yes = "long",
        no = "short"
      ),
      season_start = as.Date(glue::glue("{year}-04-01")),
      season_end = as.Date(glue::glue("{year}-10-01")),
      date_missed_season_first = pmax(date_listed, season_start),
      date_missed_season_last = dplyr::coalesce(pmin(date_return, season_end), season_end),
      season_days_missed = as.integer(date_missed_season_last - date_missed_season_first)
    )
  
  if (only_majors) {
    injury_list_raw <- injury_list_raw |>
      dplyr::filter(`Level of Play` == "ML")
  }

  if (only_new_injury) {

    injury_listed_early <- injury_list_raw |>
      # Take the first injury list placement for each player-year
      dplyr::group_by(player_id, year) |>
      dplyr::arrange(date_listed) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        date_cutoff = dplyr::case_when(
          `Level of Play` == "ML" & year == 2022 ~ "2022-04-07",
          `Level of Play` == "ML" ~ glue::glue("{year}-04-01"),
          `Level of Play` %in% c("1A", "1F", "2A", "3A") & year == 2021 ~ "2021-05-07",
          `Level of Play` %in% c("1A", "1F", "2A", "3A") ~ glue::glue("{year}-04-09"),
          `Level of Play` == "R" & year == 2024 ~ "2024-05-05",
          `Level of Play` == "R" & year == 2021 ~ "2021-06-29",
          `Level of Play` == "DS" & year == 2021 ~ "2021-07-13",
          `Level of Play` %in% c("R", "DS") ~ glue::glue("{year}-06-07")
        )
      ) |>
      dplyr::filter(date_listed < date_cutoff)

    injury_return_late <- injury_list_raw |>
      # Take the last injury list reinstatement for each player-year
      dplyr::group_by(player_id, year) |>
      dplyr::arrange(dplyr::desc(date_listed)) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        date_cutoff = dplyr::case_when(
          `Level of Play` == "ML" ~ glue::glue("{year}-09-29"),
          TRUE ~ glue::glue("{year}-09-01")
        ),
        next_year = year + 1
      ) |>
      dplyr::filter(is.na(date_return) | (date_return > date_cutoff))

    injury_continuation <- injury_listed_early |>
      dplyr::inner_join(
        y = injury_return_late,
        by = c("player_id", "body_part", "year" = "next_year"),
        suffix = c("", "_next")
      )
  }

  injury_list <- injury_list_raw |>
    dplyr::mutate(
      is_pitching_arm = (substring(body_side, 1, 1) == substring(position, 1, 1)) &
        (body_part %in% c("Arm/Elbow", "Shoulder")),
      level = ifelse(`Level of Play` == "ML", "majors", "minors"),
      date = date_listed
    )

  if (only_new_injury) {
    injury_list <- injury_list |>
      dplyr::anti_join(injury_continuation, by = c("player_id", "date_listed"))
  }

  if (only_pitching_arm) {
    injury_list <- injury_list |>
      dplyr::filter(is_pitching_arm)
  }
  
  return(injury_list)
}
