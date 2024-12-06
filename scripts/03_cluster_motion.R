source("scripts/02_identify_motion.R")

# get players that did not cross LOS when qb event happens
plays_cross_los_indicator <- tracking_players_motion_at_snap |>
  filter(!is.na(frame_qb_event)) |>
  left_join(select(play_context, gameId, playId, yardline_100)) |>
  mutate(x_los = 110 - yardline_100) |>
  filter(frameId >= frame_snap & frameId <= frame_qb_event) |>
  group_by(gameId, playId, nflId) |>
  summarize(n_cross_los = sum(x > x_los)) |>
  ungroup() |>
  mutate(i_cross_loss = ifelse(n_cross_los == 0, 0, 1)) |>
  select(-n_cross_los)

# what is the first frame when they cross LOS?
plays_cross_los_frame <- tracking_players_motion_at_snap |>
  filter(!is.na(frame_qb_event)) |>
  left_join(select(play_context, gameId, playId, yardline_100)) |>
  mutate(x_los = 110 - yardline_100) |>
  filter(frameId >= frame_snap & frameId <= frame_qb_event) |>
  filter(x > x_los) |>
  group_by(gameId, playId, nflId) |>
  slice_min(frameId) |>
  ungroup() |>
  select(gameId, playId, nflId, frame_cross_los = frameId)

# distribution for time between snap from snap to moment of crossing the LOS 
# find a threshold that captures most of the values
plays_cross_los_frame |>
  inner_join(plays_motion) |>
  mutate(frame_snap_to_cross_los = frame_cross_los - frame_snap) |>
  ggplot(aes(frame_snap_to_cross_los)) +
  geom_histogram() +
  geom_vline(xintercept = 30, linetype = "dashed")


# derive features for clustering

# get location for ball snapper (center)
# football is not reliable
# use these instead (for frames before snap)
# y_center = y_passer
# x_center = x_los

# get the passer for the play
plays_passer <- play_context |> 
  select(gameId, playId, passer_player_id) |> 
  left_join(select(nflreadr::load_players(), 
                   passer_player_id = gsis_id, nflId = gsis_it_id)) |> 
  select(-passer_player_id) |> 
  inner_join(distinct(plays_motion, gameId, playId))

# tracking features for passer
tracking_passer <- tracking |> 
  inner_join(plays_passer) |> 
  left_join(select(play_context, gameId, playId, yardline_100)) |>
  mutate(x_los = 110 - yardline_100) |> 
  select(gameId, playId, frameId, x_los, y_passer = y)

# get player location tracking features
plays_locations <- plays_motion |>
  inner_join(plays_cross_los_indicator) |>
  left_join(plays_cross_los_frame) |>
  mutate(frame_end = ifelse(i_cross_loss == 0, frame_qb_event, frame_cross_los),
         frame_end_cap = ifelse(i_cross_loss == 0 & frame_end - frame_snap > 30, 30, frame_end)) |>
  select(gameId, playId, nflId, 
         frame_line_set, frame_motion, frame_snap, frame_end = frame_end_cap) |> 
  pivot_longer(starts_with("frame_"), 
               values_to = "frameId", 
               names_to = "frame_event",
               names_prefix = "frame_") |> 
  inner_join(tracking_players_motion_at_snap) |> 
  select(gameId, playId, nflId, frameId, frame_event, x, y) |> 
  left_join(tracking_passer) |> 
  pivot_wider(id_cols = c(gameId, playId, nflId),
              names_from = frame_event,
              values_from = c(frameId, x, y, x_los, y_passer),
              values_fn = list) |> 
  unnest(cols = contains("_")) |> 
  mutate(x_center_before_snap = x_los_motion, 
         # use either y_passer_motion, y_passer_line_set, y_passer_snap - basically the same
         y_center_before_snap = y_passer_motion, 
         y_change_start = abs(y_motion - y_center_before_snap),
         y_change_end = abs(y_end - y_center_before_snap),
         x_change_snap = abs(x_snap - x_center_before_snap),
         y_change_snap = abs(y_snap - y_center_before_snap))

# plays_locations |> 
#   select(y_passer_motion, y_passer_line_set, y_passer_snap) |> 
#   pairs()

# perform clustering with a Gaussian mixture model
library(mclust)
set.seed(5)
motion_mclust <- plays_locations |> 
  select(contains("_change_")) |> 
  Mclust(G = 3:12)

motion_mclust |> 
  summary()

# choose optimal number of clusters using BIC
motion_mclust |> 
  plot(what = "BIC")

# motion_mclust |> broom::tidy()
# motion_mclust |> broom::augment(plays_locations)

plays_motion_clusters <- plays_locations |> 
  mutate(cluster = motion_mclust$classification)

# plot player trajectories for each cluster
# 1 exit 2 over 3 wider exit? 4 orbit 5 glide 6 fly/jet 7 in
plays_motion_clusters |> 
  mutate(curve_id = row_number()) |> 
  select(gameId, playId, nflId, frameId_motion, frameId_end, cluster, curve_id) |> 
  left_join(tracking_players_motion_at_snap) |> 
  mutate(frame_color = ifelse(frameId == frameId_motion, "red", NA),
         frame_color = ifelse(frameId == frame_snap, "blue", frame_color)) |> 
  filter(frameId >= frameId_motion & frameId <= frame_snap) |> 
  ggplot(aes(y, x, group = curve_id)) +
  geom_point(aes(color = I(frame_color)), size = 0.8, alpha = 0.5) +
  geom_path(alpha = 0.3) + 
  facet_wrap(~ cluster) +
  labs(x = "along the sideline", y = "yardline")
