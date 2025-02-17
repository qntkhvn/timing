receiver_filtered <- plays_snap_timing |> 
  distinct(gameId, playId, nflId) |> 
  count(nflId) |> 
  filter(n >= 20)

receiver_teams <- tracking |> 
  filter(nflId %in% receiver_filtered$nflId) |> 
  distinct(nflId, club) |>
  group_by(nflId) |> 
  summarize(club = str_c(club, collapse = "/"))

receiver_posterior <- snap_timing_fit |> 
  spread_draws(r_nflId[nflId, term]) |> 
  left_join(players) |> 
  filter(nflId %in% receiver_filtered$nflId) |> 
  left_join(receiver_teams) |> 
  mutate(displayName = str_replace(displayName, "St. ", "St")) |> 
  separate(displayName, into = c("first_name", "last_name"), sep = "\\s") |> 
  mutate(receiver_name = str_c(str_sub(first_name, 1, 1), ".", last_name),
         receiver_name = str_replace(receiver_name, "St", "St. ")) |>  
  mutate(receiver_name_team = str_c(str_replace(receiver_name, "\\.", "\\. "), " (", club, ")")) |> 
  group_by(nflId, receiver_name, receiver_name_team, position) |> 
  summarize(posterior_mean_receiver = mean(r_nflId)) |> 
  ungroup() |> 
  arrange(posterior_mean_receiver)

# receiver_posterior_top_bottom_10 <- receiver_posterior |> slice(c(1:10, (n()-9):n()))

# receiver_posterior_top_bottom_10 <- receiver_posterior

receiver_posteriors_slab <- snap_timing_fit |> 
  spread_draws(r_nflId[nflId, term]) |> 
  left_join(players) |> 
  filter(nflId %in% receiver_posterior$nflId) |> 
  left_join(receiver_teams) |> 
  mutate(displayName = str_replace(displayName, "St. ", "St")) |> 
  separate(displayName, into = c("first_name", "last_name"), sep = "\\s") |> 
  mutate(receiver_name = str_c(str_sub(first_name, 1, 1), ".", last_name),
         receiver_name = str_replace(receiver_name, "St", "St. ")) |>  
  mutate(receiver_name_team = str_c(str_replace(receiver_name, "\\.", "\\. "), " (", club, ")"),
         receiver_name_team = factor(receiver_name_team, receiver_posterior$receiver_name_team)) |> 
  ggplot(aes(x = r_nflId, y = receiver_name_team)) +
  stat_slab(alpha = 0.3, scale = 0.93) +
  stat_interval(alpha = 0.8) +
  stat_summary(geom = "point", fun = mean, size = 0.8) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"),
                     labels = c("95%", "80%", "50%")) +
  coord_cartesian(xlim = c(-0.7, 0.5)) +
  labs(x = "Motion player random effect",
       y = NULL,
       color = "Credible Interval") +
  theme_minimal(base_family = "Fira Sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray95"),
        text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", size = rel(1.1), color = "black"),
        axis.title.x = element_text(face = "bold"),
        legend.position = "bottom",
        legend.margin = margin(t = -3))

ggsave(
  "figures/receiver_posteriors_slab.png",
  plot = receiver_posteriors_slab,
  width = 5,
  height = 7,
  bg = "white"
)


# qb mean posterior -------------------------------------------------------

qb_filtered <- plays_snap_timing |> 
  distinct(gameId, playId, passer_player_id, passer_player_name) |> 
  count(passer_player_id, passer_player_name) |>
  filter(n >= 50)

library(tidybayes)
qb_mean_posterior <- snap_timing_fit |> 
  spread_draws(r_passer_player_id[passer_player_id, term]) |>
  left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(distinct(play_context, passer_player_name, posteam)) |> 
  mutate(passer_name_team = str_c(str_replace(passer_player_name, "\\.", "\\. "), " (", posteam, ")")) |> 
  group_by(passer_player_id, passer_player_name, passer_name_team) |> 
  summarize(posterior_mean_qb = mean(r_passer_player_id)) |> 
  ungroup() |> 
  arrange(posterior_mean_qb)

library(ggdist)
qb_mean_posteriors_slab <- snap_timing_fit |> 
  spread_draws(r_passer_player_id[passer_player_id, term]) |>
  left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(distinct(play_context, passer_player_name, posteam)) |> 
  mutate(passer_name_team = str_c(str_replace(passer_player_name, "\\.", "\\. "), " (", posteam, ")"),
         passer_name_team = factor(passer_name_team, qb_mean_posterior$passer_name_team)) |> 
  ggplot(aes(x = r_passer_player_id, y = passer_name_team)) +
  stat_slab(alpha = 0.3, scale = 0.93) +
  stat_interval(alpha = 0.8) +
  stat_summary(geom = "point", fun = mean, size = 0.8) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"),
                     labels = c("95%", "80%", "50%")) +
  coord_cartesian(xlim = c(-0.5, 0.5)) +
  labs(x = "Quarterback random effect",
       y = NULL,
       color = "Credible Interval") +
  theme_minimal(base_family = "Fira Sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray95"),
        text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", size = rel(1.1), color = "black"),
        axis.title.x = element_text(face = "bold"),
        legend.position = "bottom",
        legend.margin = margin(t = -3))

ggsave(
  "figures/qb_mean_posteriors_slab.png",
  plot = qb_mean_posteriors_slab,
  width = 5,
  height = 7,
  bg = "white"
)
