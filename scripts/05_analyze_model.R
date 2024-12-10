source("scripts/04_fit_model.R")

# analyze qb shape

qb_filtered <- plays_snap_timing |> 
  distinct(gameId, playId, passer_player_id, passer_player_name) |> 
  count(passer_player_id, passer_player_name) |>
  filter(n >= 50)

library(tidybayes)
qb_shape_posterior <- snap_timing_fit |> 
  spread_draws(r_passer_player_id__shape[passer_player_id, term]) |>
  left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(distinct(play_context, passer_player_name, posteam)) |> 
  mutate(passer_name_team = str_c(str_replace(passer_player_name, "\\.", "\\. "), " (", posteam, ")")) |> 
  group_by(passer_player_id, passer_player_name, passer_name_team) |> 
  summarize(posterior_mean_shape = mean(r_passer_player_id__shape)) |> 
  ungroup() |> 
  arrange(posterior_mean_shape)

library(ggdist)
posteriors_slab <- snap_timing_fit |> 
  spread_draws(r_passer_player_id__shape[passer_player_id, term]) |>
  left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(distinct(play_context, passer_player_name, posteam)) |> 
  mutate(passer_name_team = str_c(str_replace(passer_player_name, "\\.", "\\. "), " (", posteam, ")"),
         passer_name_team = factor(passer_name_team, qb_shape_posterior$passer_name_team)) |> 
  ggplot(aes(x = r_passer_player_id__shape, y = passer_name_team)) +
  stat_slab(alpha = 0.3, scale = 0.93) +
  stat_interval(alpha = 0.8) +
  stat_summary(geom = "point", fun = mean, size = 0.8) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"),
                     labels = c("95%", "80%", "50%")) +
  coord_cartesian(xlim = c(-0.9, 0.9)) +
  labs(x = "QB shape random effect",
       y = NULL,
       color = "Credible Interval",
       title = "Posterior distributions of QB shape random effect",
       subtitle = "For plays with players in motion at snap & running a route\nand QBs with at least 50 snaps played") +
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

# write_rds(posteriors_slab, "figures/posteriors_slab.rds", compress = "gz")


plays_havoc <- player_play |> 
  mutate(havoc = (passDefensed + forcedFumbleAsDefense + tackleForALoss + hadInterception +
                    (halfSackYardsAsDefense < 0) + (sackYardsAsDefense < 0) + causedPressure) > 0) |>
  group_by(gameId, playId) |>
  summarize(havoc = sum(havoc) > 0) |> 
  ungroup()

plays_havoc_rate_motion <- plays_snap_timing |>  
  inner_join(plays_havoc) |> 
  group_by(passer_player_id, posteam) |> 
  summarize(havoc_rate_motion = mean(havoc)) |> 
  ungroup()

plays_havoc_rate_overall <- play_context |> 
  filter(play_type == "pass") |> 
  inner_join(plays_havoc) |> 
  group_by(passer_player_id, posteam) |> 
  summarize(havoc_rate_overall = mean(havoc)) |> 
  ungroup()
  
corr_havoc <- plays_havoc_rate_motion |> 
  inner_join(qb_shape_posterior) |> 
  left_join(plays_havoc_rate_overall) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(nflreadr::load_teams(), by = c("posteam" = "team_abbr")) |> 
  pivot_longer(contains("havoc_rate"),
               names_to = "play_subset",
               names_prefix = "havoc_rate_",
               values_to = "havoc_rate") |> 
  mutate(
    passer_player_name = str_replace(passer_player_name, "\\.", "\\. "),
    play_subset = ifelse(play_subset == "motion", 
                         "Considered motion plays (r = –0.40)", "All pass plays (r = –0.52)")
  ) |> 
  # group_by(play_subset) |> summarize(cor(posterior_mean_shape, havoc_rate))
  ggplot(aes(posterior_mean_shape, havoc_rate)) +
  geom_smooth(method = lm, se = FALSE,
              color = "gray",
              xseq = c(-0.45, 0.38),
              linetype = "dashed") +
  geom_point(alpha = 0.6, size = 3.5, aes(color = I(team_color))) +
  ggrepel::geom_text_repel(aes(label = passer_player_name), 
                           family = "Fira Sans", size = rel(3.2), seed = 31) +
  facet_wrap(~ play_subset) +
  labs(x = "Posterior mean for QB shape random effect",
       y = "Havoc rate") +
  theme_light() +
  annotate("segment", x = 0.26, y = 0.555, xend = 0.395, yend = 0.555, linewidth = 0.8,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = -0.18, y = 0.555, hjust = 0,
           label = "Higher snap timing variability",
           family = "Fira Sans", fontface = "bold", size = rel(4)) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Fira Sans"),
        axis.title = element_text(face = "bold", size = rel(1.2)),
        strip.text = element_text(face = "bold", size = rel(1.1), color = "black"),
        strip.background = element_rect(fill = "gray90", color = NA),
        plot.title = element_text(hjust = 0.5))

# write_rds(corr_havoc, "figures/corr_havoc.rds", compress = "gz")

# yards per attempt
# success rate
# time to throw


# look at the mean parameter

qb_mean_posterior <- snap_timing_fit |> 
  spread_draws(r_passer_player_id[passer_player_id, term]) |>
  left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(distinct(play_context, passer_player_name, posteam)) |> 
  mutate(passer_name_team = str_c(str_replace(passer_player_name, "\\.", "\\. "), " (", posteam, ")")) |> 
  group_by(passer_player_id, passer_player_name, passer_name_team) |> 
  summarize(posterior_mean_mu = mean(r_passer_player_id)) |> 
  ungroup() |> 
  arrange(posterior_mean_mu)

# no relationship between mean and variance parameter
qb_mean_posterior |> 
  left_join(qb_shape_posterior) |> 
  ggplot(aes(posterior_mean_mu, posterior_mean_shape)) +
  geom_point()


# look at receivers

receiver_filtered <- plays_snap_timing |> 
  distinct(gameId, playId, nflId) |> 
  count(nflId) |> 
  filter(n > 15)

receiver_posterior <- snap_timing_fit |> 
  spread_draws(r_nflId[nflId, term]) |> 
  left_join(players) |> 
  filter(nflId %in% receiver_filtered$nflId) |> 
  group_by(nflId, displayName, position) |> 
  summarize(posterior_mean = mean(r_nflId)) |> 
  ungroup() |> 
  arrange(posterior_mean)


snap_timing_fit |> 
  spread_draws(r_nflId[nflId, term]) |> 
  left_join(players) |> 
  filter(nflId %in% receiver_filtered$nflId) |> 
  mutate(displayName = factor(displayName, receiver_posterior$displayName)) |> 
  ggplot(aes(x = r_nflId, y = displayName, fill = position)) +
  ggridges::geom_density_ridges(rel_min_height = 0.05, alpha = 0.5) +
  # scale_fill_manual(values = c("gray", "darkblue", "darkred", "darkorange")) +
  scale_fill_manual(values = c("gold", "red", "purple", "gray")) +
  # stat_interval(alpha = 0.8) +
  # stat_summary(geom = "point", fun = mean, size = 0.8) +
  # scale_color_manual(values = MetBrewer::met.brewer("VanGogh3"),
  #                    labels = c("95%", "80%", "50%")) +
  labs(x = "player random effect",
       y = NULL,
       subtitle = "Players with more than 15 motions") +
  theme_minimal()

