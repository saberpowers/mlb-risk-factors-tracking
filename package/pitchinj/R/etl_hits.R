#' Extract, transform and load pitching injuries from the MLB Health and Injury Tracking System
#' 
#' This function requires that you have hits.csv available under input/data/injury. The hits.csv
#' file is available in our shared Box folder.
#' 
#' @param min_days_until_clearance integer, injury records with fewer days until clearance than
#'   this threshold are filtered out
#'
#' @return a table of HITS injury records
#'  
#' @export
etl_hits <- function(min_days_until_clearance = 7) {

  hits_raw <- data.table::fread("input/data/injury/hits.csv")
  
  hits <- hits_raw |>
    dplyr::mutate(
      player_id = `Player ID`,
      date = as.Date(`Injury Date`, format = "%m/%d/%y"),
      year = lubridate::year(date),
      level = ifelse(`Event Level of Play` == "MLB", "majors", "minors"),
      body_side = `Body Side`,
      body_part = `Body Region`,
      position = `BOC Pos`,
      # We need a `severity` column to exist for compatability with downstream code,
      # but the codebase doesn't actually use it.
      severity = "yes",
      # When Days Until Clearance is "-", it's always a long injury (not cleared during season)
      `Days Until Clearance` =
        ifelse(
          test = `Days Until Clearance` == "-",
          yes = "365",
          no = `Days Until Clearance`
        ) |>
        as.integer()
    ) |>
    dplyr::filter(
      `Days Until Clearance` >= min_days_until_clearance,
      `Injury Activity` %in% c("Pitching", "Throwing", "Other") |
        `Injury Type` %in% "Overuse" |
        `Mechanism Of Injury` %in% c("Overuse Gradual Onset", "Non Contact"),
      !grepl("^Contact", `Mechanism Of Injury`),
      !grepl("accident", tolower(`Injury Activity Detail`)),
      !grepl("scooter", tolower(`Injury Activity Detail`)),
      !grepl("stairs", tolower(`Injury Activity Detail`)),
      !grepl("fell", tolower(`Injury Activity Detail`)),
      !grepl("fall", tolower(`Injury Activity Detail`)),
      !grepl("fight", tolower(`Injury Activity Detail`)),
      !grepl("punch", tolower(`Injury Activity Detail`)),
      !grepl("brawl", tolower(`Injury Activity Detail`)),
      !grepl("altercation", tolower(`Injury Activity Detail`)),
      !grepl("robbery", tolower(`Injury Activity Detail`)),
      !grepl("celebration", tolower(`Injury Activity Detail`)),
      !grepl("bathroom", tolower(`Injury Activity Detail`)),
      !grepl("burn", tolower(`Injury Activity Detail`)),
      !grepl("furniture", tolower(`Injury Activity Detail`)),
      !grepl("^Contact", `Mechanism Of Injury Detail`),
      !grepl("accident", tolower(`Mechanism Of Injury Detail`)),
      !grepl("crash", tolower(`Mechanism Of Injury Detail`)),
      !grepl("contusion", tolower(`Mechanism Of Injury Detail`)),
      !grepl("fall", tolower(`Mechanism Of Injury Detail`)),
      !(
        grepl("contusion", tolower(`Event Description`)) &
        grepl("contusion", tolower(`SMDCS Description`))
      )
    )

  return(hits)
}