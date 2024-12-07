source("scripts/04_fit_model.R")

# game, play, motion player

plays_snap_timing |> 
  filter(passer_player_name == "P.Mahomes") |> 
  left_join(select(players, nflId, displayName)) |> 
  select(gameId, playId, cluster, motion_player = displayName, receiver_player_name, frame_between) |> 
  inner_join(select(plays_use_motion_tag, gameId, playId)) |> 
  View()




ex1 <- tracking |> 
  filter(gameId == 2022092502, playId == 1180) |> 
  mutate(first_frame = frameId[which(event == "line_set")][1],
         last_frame = frameId[which(event == "tackle")][1]) |> 
  filter(frameId >= first_frame & frameId <= last_frame)

desc1 <- plays |> 
  filter(gameId == 2022092502, playId == 1180) |> 
  pull(playDescription)

# 20 to 45
library(gganimate)
ex1 |> 
  mutate(
    pt_size = ifelse(club == "football", 2.5, 5),
    pt_fill = case_when(
      club == "IND" ~ "#002C5F",
      club == "KC" & nflId == "40011" ~ "gold",
      club == "KC" & nflId != "47839" ~ "white",
      club == "football" ~ "#663831"
    )
  ) |> 
  ggplot(aes(x - 10, y)) +
  geom_point(aes(fill = pt_fill, size = pt_size), shape = 21) +
  scale_fill_identity() +
  scale_size_identity() +
  coord_flip() +
  scale_y_reverse() +
  transition_time(frameId) +
  theme(panel.background = element_rect(fill = "springgreen3"),
        axis.text.x = element_blank(),
        axis.title = element_blank()) +
  labs(title = "KC vs IND Week 3",
       subtitle = "3.6s between motion & snap. In motion: Travis Kelce. Motion type: Glide",
       caption = str_remove_all(desc1, "\\.\\s.*"))




ex2 <- tracking |> 
  filter(gameId == 2022102310, playId == 3897) |> 
  mutate(first_frame = frameId[which(event == "line_set")][1],
         last_frame = frameId[which(event == "touchdown")][1]) |> 
  filter(frameId >= first_frame & frameId <= last_frame)

desc2 <- plays |> 
  filter(gameId == 2022102310, playId == 3897) |> 
  pull(playDescription)

ex2 |> 
  mutate(
    pt_size = ifelse(club == "football", 2.5, 5),
    pt_fill = case_when(
      club == "SF" ~ "#AA0000",
      club == "KC" & nflId == "47839" ~ "gold",
      club == "KC" & nflId != "47839" ~ "white",
      club == "football" ~ "#663831"
    )
  ) |> 
  ggplot(aes(x, y)) +
  geom_point(aes(fill = pt_fill, size = pt_size), shape = 21) +
  scale_fill_identity() +
  scale_size_identity() +
  coord_flip() +
  scale_y_reverse() +
  scale_x_continuous(breaks = seq(60, 100, 10), labels = c(seq(40, 10, -10), "G")) +
  transition_time(frameId) +
  theme(panel.background = element_rect(fill = "springgreen3"),
        axis.text.x = element_blank(),
        axis.title = element_blank()) +
  labs(title = "KC vs SF Week 7",
       subtitle = "0.5s between motion & snap. In motion: Mecole Hardman. Motion type: Glide",
       caption = desc2)