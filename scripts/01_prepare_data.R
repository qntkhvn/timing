library(tidyverse)

# tracking <- read_csv("data/tracking_week_1.csv") |> 
#   bind_rows(read_csv("data/tracking_week_2.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_3.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_4.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_5.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_6.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_7.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_8.csv")) |> 
#   bind_rows(read_csv("data/tracking_week_9.csv"))
# arrow::write_parquet(tracking, "data/tracking.parquet")

tracking <- arrow::read_parquet("data/tracking.parquet")
games <- read_csv("data/games.csv")
plays <- read_csv("data/plays.csv")
players <- read_csv("data/players.csv")
player_play <- read_csv("data/player_play.csv")

# player_play for offense only
# correct some missing data
# https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/discussion/548627
player_play_offense <- player_play |> 
  left_join(select(plays, gameId, playId, possessionTeam)) |> 
  filter(teamAbbr == possessionTeam) |> 
  mutate(
    inMotionAtBallSnap = ifelse(is.na(inMotionAtBallSnap), FALSE, inMotionAtBallSnap),
    shiftSinceLineset = ifelse(is.na(shiftSinceLineset), FALSE, shiftSinceLineset),
    motionSinceLineset = ifelse(is.na(motionSinceLineset), 
                                inMotionAtBallSnap | shiftSinceLineset, 
                                motionSinceLineset)
  )

# standardize coordinates
tracking <- tracking |>
  mutate(
    x = ifelse(playDirection == "left", 120 - x, x),
    y = ifelse(playDirection == "left", 160 / 3 - y, y),
    dir = ifelse(playDirection == "left", dir + 180, dir),
    dir = ifelse(dir > 360, dir - 360, dir),
    o = ifelse(playDirection == "left", o + 180, o),
    o = ifelse(o > 360, o - 360, o)
  )

# get players in motion at snap AND running route
players_motion_at_snap <- player_play_offense |> 
  filter(inMotionAtBallSnap & wasRunningRoute == 1) |> 
  distinct(gameId, playId, nflId) |> 
  add_count(gameId, playId, name = "n_motion_at_snap")

# for each play, how many players have any type of motion since line set?
plays_motion_since_line_set <- player_play_offense |> 
  group_by(gameId, playId) |> 
  summarize(n_motion_since_line_set = sum(motionSinceLineset, na.rm = TRUE)) |> 
  ungroup()

players_motion_at_snap <- players_motion_at_snap |> 
  left_join(plays_motion_since_line_set)

# get tracking data for those players
tracking_players_motion_at_snap <- tracking |> 
  inner_join(players_motion_at_snap)

# get the frame for different annotated events for each play
qb_events <- c("pass_forward", "qb_sack", "qb_strip_sack", "fumble", 
               "fumble_defense_recovered", "pass_shovel", "pass_tipped")
tracking_players_motion_at_snap <- tracking_players_motion_at_snap |> 
  group_by(gameId, playId) |> 
  mutate(
    frame_line_set = frameId[which(event == "line_set")][1],
    frame_man_in_motion = frameId[which(event == "man_in_motion")][1],
    frame_snap = frameId[which(frameType == "SNAP")][1],
    frame_qb_event = frameId[which(event %in% qb_events)][1]
  ) |> 
  ungroup() 

# also get play context from nflreadr
play_context <- nflreadr::load_pbp(2022) |> 
  mutate(gameId = as.numeric(old_game_id),
         playId = play_id)
