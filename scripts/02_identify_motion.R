source("scripts/01_prepare_data.R")

# plays where players in motion at snap is also only one in motion since line set
# use man_in_motion tag
plays_use_motion_tag <- tracking_players_motion_at_snap |> 
  filter(n_motion_at_snap == 1 & n_motion_since_line_set == 1 & event == "man_in_motion") |> 
  group_by(gameId, playId) |> 
  slice_min(frameId) |> # 2022100205 3671: 2 man_in_motion events
  ungroup() |> 
  select(gameId:nflId, frame_line_set, frame_motion = frameId, frame_snap, frame_qb_event) |> 
  filter(frame_motion <= frame_snap)

# plays_use_motion_tag |>
#   distinct(gameId, playId, nflId, frame_motion) |> 
#   inner_join(tracking_players_motion_at_snap) |> 
#   group_by(gameId, playId, nflId) |> 
#   mutate(max_s = max(s, na.rm = TRUE)) |>
#   ungroup() |> 
#   filter(frameId == frame_motion) |> 
#   mutate(frac_s = s / max_s) |> 
#   write_rds("data/motion_speed_ratio.rds")

# for these play, observe the distribution of ratio between
# speed at man_in_motion and max speed
plays_use_motion_tag |>
  distinct(gameId, playId, nflId, frame_motion) |> 
  inner_join(tracking_players_motion_at_snap) |> 
  group_by(gameId, playId, nflId) |> 
  mutate(max_s = max(s, na.rm = TRUE)) |>
  ungroup() |> 
  filter(frameId == frame_motion) |> 
  mutate(frac_s = s / max_s) |> 
  # pull(frac_s) |> summary()
  ggplot(aes(frac_s)) +
  geom_histogram(bins = 30, fill = "gray", color = "gray40") +
  geom_vline(xintercept = 0.453, linetype = "dashed")

#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2739  0.4538  0.4534  0.6360  1.0000 

# for remaining plays, the start of motion is first frame
# where the player reaches > 45% max speed
plays_motion_tag_manually <- tracking_players_motion_at_snap |> 
  anti_join(select(plays_use_motion_tag, ends_with("Id"))) |> 
  filter(!is.na(frame_line_set)) |> 
  filter(frameId > frame_line_set & frameId < frame_snap) |> 
  group_by(gameId, playId, nflId) |>
  mutate(max_s = max(s, na.rm = TRUE)) |>
  ungroup() |>
  mutate(frac_s = s / max_s) |> 
  filter(frac_s > 0.453) |>
  group_by(gameId, playId, nflId) |>
  slice_min(frameId) |> 
  ungroup() |>
  select(gameId:nflId, frame_line_set, frame_motion = frameId, frame_snap, frame_qb_event)

# combine the two cases into one single table
plays_motion <- plays_use_motion_tag |> 
  bind_rows(plays_motion_tag_manually) |>
  mutate(frame_between = frame_snap - frame_motion)
