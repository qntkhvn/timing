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

fit <- brm(
  bf(y ~ factor(var1) + var2 + factor(var3) + factor(var3) + var4 + factor(var5) +
      (1 | group1) + (1 | group2) + (1 | group3),
    shape ~ (1 | group1)),
  data = df)

# model diagnostics
snap_timing_fit |> 
  brms::rhat() |> 
  summary()

snap_timing_fit |> 
  bayestestR::effective_sample(effects = "all")

snap_timing_fit |> 
  bayesplot::pp_check()


