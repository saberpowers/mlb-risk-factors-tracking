
# Download player table ----

cluster <- parallel::makeCluster(parallel::detectCores())
player <- sabRmetrics::download_player(year = 2017:2026, cl = cluster)
parallel::stopCluster(cluster)
data.table::fwrite(player, file = "input/data/player.csv")


# Download season summary data ----

cluster <- parallel::makeCluster(parallel::detectCores())
pitcher_season_summary <- sabRmetrics::download_season_summary(
  year = 2009:2026,
  level = c("MLB", "AAA", "AA", "A+", "A", "SS", "CL", "DSL"),
  position = "pitching",
  game_type = c("R", "P"),
  cl = cluster
)
parallel::stopCluster(cluster)
pitcher_season_summary |>
  dplyr::rename(games = gamesPitched, batters_faced = battersFaced, pitches = numberOfPitches) |>
  dplyr::select(year, level, league_id, player_id, games, batters_faced, pitches) |>
  data.table::fwrite("input/data/pitcher_season_summary.csv")


# Download pitch-by-pitch tracking data from baseballsavant ----

years <- 2017
cluster <- parallel::makeCluster(parallel::detectCores())
for (year in years) {
  logger::log_info("Downloading baseballsavant data for {year}...")
  baseballsavant <- sabRmetrics::download_baseballsavant(
    start_date = glue::glue("{year}-01-01"),
    end_date = glue::glue("{year}-12-31"),
    game_type = c("R", "F", "D", "L", "W", "S"),  # regular season, postseason, spring training
    cl = cluster
  )
  baseballsavant |>
    dplyr::select(game_id, game_date, game_type, event_index, pitch_number,
      home_team, away_team, batter_id, bat_side, pitcher_id, pitch_hand,
      inning, half_inning, outs, balls, strikes,
      pitch_type, arm_angle, ax, ay, az, vx0, vy0, vz0, release_pos_x, release_pos_y, release_pos_z,
      release_speed, plate_x, plate_z, release_spin_rate,
    ) |>
    data.table::fwrite(glue::glue("data/baseballsavant/{year}.csv"))
}
parallel::stopCluster(cluster)
