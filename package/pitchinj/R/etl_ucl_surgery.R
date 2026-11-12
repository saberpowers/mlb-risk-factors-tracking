#' Extract, transform and load UCL surgery data
#' 
#' This function reads UCL injury data from multiple sources and combines them.
#' 
#' @param min_year integer, minimum year for which to return data
#' 
#' @return a table of UCL injuries indexed by player_id and year
#' 
#' @export
#' 
etl_ucl_surgery <- function(min_year = 2018) {

  ucl_public_tommyjohn <- data.table::fread("input/data/injury/ucl_public_tommyjohn.csv") |>
    dplyr::mutate(surgery = "tommy_john")
  ucl_public_internalbrace <- data.table::fread("input/data/injury/ucl_public_internalbrace.csv") |>
    dplyr::mutate(surgery = "internal_brace")
  ucl_public <- ucl_public_tommyjohn |>
    dplyr::filter(position == "P") |>
    dplyr::bind_rows(ucl_public_internalbrace) |>
    dplyr::mutate(
      date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
      year = lubridate::year(date_surgery),
      level = dplyr::case_when(
        level == "MLB" ~ "majors",
        level %in% c("AAA", "AA", "A+", "A", "A-", "Rk") ~ "minors",
        level %in% c("Coll", "HS") ~ "amateur"
      )
    ) |>
    dplyr::select(player_id, level, date_surgery, surgery, year, position)

  ucl_verified <- data.table::fread("input/data/injury/ucl_verified.csv") |>
    dplyr::mutate(
      date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
      level = dplyr::case_when(
        level == "Major" ~ "majors",
        level == "Minor" ~ "minors",
        level == "Amateur" ~ "amateur"
      )
    )

  ucl_surgery <- ucl_verified |>
    dplyr::full_join(ucl_public,
      by = c("player_id", "year"),
      suffix = c("_verified", "_public")
    ) |>
    dplyr::mutate(
      date = dplyr::coalesce(date_surgery_verified, date_surgery_public),
      level = dplyr::coalesce(level_verified, level_public),
      severity = "long"
    ) |>
    dplyr::filter(
      year >= min_year,         # match injury list data, which begins in 2018
      level != "amateur"    # for now, we are focused only on pro pitchers
    ) |>
    dplyr::select(player_id, year, date, level, severity)

  return(ucl_surgery)
}
