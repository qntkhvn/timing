source("scripts/03_cluster_motion.R")

# final data for modeling
plays_snap_timing <- plays_motion |> 
  left_join(play_context) |> 
  left_join(plays) |> 
  left_join(select(players, nflId, position)) |> 
  left_join(plays_motion_since_line_set) |> 
  inner_join(select(player_play_offense, gameId, playId, nflId, routeRan)) |> 
  inner_join(select(plays_motion_clusters, gameId, playId, nflId, cluster)) |> 
  mutate(play_clock_at_motion = playClockAtSnap + (frame_between / 10),
         position = ifelse(position == "QB", "TE", position), # Taysom Hill
         position = ifelse(position %in% c("FB", "RB"), "B", position))

# fit model
library(brms)
snap_timing_fit <- brm(
  bf(
    # mean
    frame_between ~
      factor(down) + play_clock_at_motion + factor(posteam_timeouts_remaining) + 
      position + n_motion_since_line_set + factor(cluster) +
      (1 | passer_player_id) + (1 | nflId) + (1 | defensiveTeam),
    # variance
    shape ~ (1 | passer_player_id)
  ),
  family = Gamma(link = "log"),
  chains = 4,
  iter = 10000,
  warmup = 5000,
  seed = 3,
  cores = 4,
  backend = "cmdstanr",
  data = plays_snap_timing
)

# model diagnostics
snap_timing_fit |> 
  plot()

snap_timing_fit |> 
  brms::rhat() |> 
  summary()

snap_timing_fit |> 
  bayestestR::effective_sample(effects = "all")

snap_timing_fit |> 
  bayesplot::pp_check()

# model comparison

plays_location_change <- plays_motion |>
  inner_join(plays_frame_end) |> 
  select(gameId, playId, nflId, frame_motion, frame_end) |> 
  pivot_longer(starts_with("frame_"), values_to = "frameId") |>
  select(-name) |>
  inner_join(tracking_players_motion_at_snap) |>
  group_by(gameId, playId, nflId) |>
  summarize(y_change = max(y) - min(y)) |>
  ungroup()

plays_snap_timing_comparison <- plays_snap_timing |> 
  left_join(plays_location_change)

# fit model
library(brms)
snap_timing_fit_no_cluster <- brm(
  bf(
    # mean
    frame_between ~
      factor(down) + play_clock_at_motion + factor(posteam_timeouts_remaining) + 
      position + n_motion_since_line_set + factor(routeRan) + y_change +
      (1 | passer_player_id) + (1 | nflId) + (1 | defensiveTeam),
    # variance
    shape ~ (1 | passer_player_id)
  ),
  family = Gamma(link = "log"),
  chains = 4,
  iter = 10000,
  warmup = 5000,
  seed = 3,
  cores = 4,
  backend = "cmdstanr",
  data = plays_snap_timing_comparison
)


# validation
snap_timing_loo <- brms::loo(snap_timing_fit_no_cluster, 
                             snap_timing_fit)
