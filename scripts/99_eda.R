source("scripts/04_fit_model.R")

plays_snap_timing |> 
  ggplot(aes(frame_between)) +
  geom_histogram(bins = 30)

plays_snap_timing |> 
  add_count(passer_player_name) |> 
  filter(n >= 50) |> 
  ggplot(aes(x = frame_between)) +
  geom_histogram() +
  facet_wrap(~ passer_player_name)

plays_snap_timing |> 
  mutate(down = factor(down)) |> 
  ggplot(aes(frame_between, color = down)) +
  stat_ecdf(linewidth = 1, alpha = 0.6) +
  theme(legend.position = "bottom")

plays_snap_timing |> 
  mutate(posteam_timeouts_remaining = factor(posteam_timeouts_remaining)) |> 
  ggplot(aes(frame_between, color = posteam_timeouts_remaining)) +
  stat_ecdf(linewidth = 1) +
  theme(legend.position = "bottom")

plays_snap_timing |> 
  ggplot(aes(frame_between, color = position)) +
  stat_ecdf(linewidth = 1) +
  theme(legend.position = "bottom")

plays_snap_timing |> 
  left_join(player_play) |> 
  ggplot(aes(frame_between, color = routeRan)) +
  stat_ecdf(linewidth = 1, alpha = 0.5)

plays_snap_timing |> 
  ggplot(aes(frame_between)) +
  geom_histogram() +
  facet_wrap(~ routeRan)

plays_snap_timing |> 
  ggplot(aes(play_clock_at_motion, frame_between)) +
  geom_point()

plays_snap_timing |> 
  left_join(player_play) |> 
  ggplot(aes(frame_between, color = factor(cluster))) +
  stat_ecdf(linewidth = 1, alpha = 0.5)
