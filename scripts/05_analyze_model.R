source("scripts/04_fit_model.R")

# analyze qb shape

# plays_snap_timing |> write_rds("data/plays_snap_timing.rds", compress = "gz")
# snap_timing_fit |> write_rds("data/snap_timing_fit.rds", compress = "gz")

plays_snap_timing <- read_rds("data/plays_snap_timing.rds")
snap_timing_fit <- read_rds("data/snap_timing_fit.rds")

# plays_snap_timing |> 
#   left_join(distinct(play_context, passer_player_name, passer_player_id)) |> 
#   select(gameId:nflId, passer_player_id, passer_player_name, defensiveTeam, 
#          frame_between, down, play_clock_at_motion, posteam_timeouts_remaining,
#          position, n_motion_since_line_set, motion_cluster = cluster) |> 
#   write_csv("scripts/plays_snap_timing_demo.csv.gz")

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
  annotate("segment", x = 0.505, y = 1, xend = 0.98, yend = 1, linewidth = 0.7,
           arrow = arrow(type = "closed", length = unit(0.015, "npc"))) +
  annotate("text", x = 0.99, y = 1.8, hjust = 1, lineheight = 1,
           label = "Higher variability\nin snap timing",
           family = "Fira Sans", fontface = "bold", size = rel(3.6)) +
  labs(x = "QB shape random effect",
       y = NULL,
       color = "Credible Interval",
       title = "Posterior distributions of QB shape random effect",
       subtitle = "For plays with receivers in motion at snap & running a route\nand QBs with at least 50 pass attempts over these plays") +
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
  # group_by(play_subset) |> do(broom::tidy(cor.test(.$posterior_mean_shape, .$havoc_rate, alternative = "less")))
  ggplot(aes(posterior_mean_shape, havoc_rate)) +
  geom_smooth(method = lm, se = FALSE,
              color = "gray",
              xseq = c(-0.41, 0.33),
              linetype = "dashed") +
  geom_point(alpha = 0.6, size = 3.5, aes(color = I(team_color))) +
  ggrepel::geom_text_repel(aes(label = passer_player_name), 
                           family = "Fira Sans", size = rel(3.2), seed = 31) +
  facet_wrap(~ play_subset) +
  labs(x = "Posterior mean for QB shape random effect",
       y = "Havoc rate") +
  theme_light() +
  annotate("segment", x = 0.115, y = 0.528, xend = 0.33, yend = 0.528, linewidth = 0.7,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 0.33, y = 0.55, hjust = 1, lineheight = 1,
           label = "Higher variability\nin snap timing",
           family = "Fira Sans", fontface = "bold", size = rel(3.6)) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Fira Sans"),
        axis.title = element_text(face = "bold", size = rel(1.2)),
        strip.text = element_text(face = "bold", size = rel(1.1), color = "black"),
        strip.background = element_rect(fill = "gray90", color = NA),
        plot.title = element_text(hjust = 0.5))

# write_rds(corr_havoc, "figures/corr_havoc.rds", compress = "gz")


# supplements -------------------------------------------------------------

# yards per attempt
# success rate
# time to throw

# havoc rate vs motion rate

plays_motion_rate_overall <- player_play_offense |> 
  group_by(gameId, playId) |> 
  summarize(motion = sum(motionSinceLineset) > 0) |> 
  left_join(select(play_context, gameId, playId, posteam, 
                   passer_player_id, passer_player_name, play_type)) |> 
  filter(play_type == "pass") |> 
  group_by(passer_player_id, passer_player_name) |> 
  summarize(motion_since_line_set_rate_overall = mean(motion)) |> 
  ungroup()


corr_motion_havoc <- plays_motion_rate_overall |> 
  full_join(plays_havoc_rate_overall) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(nflreadr::load_teams(), by = c("posteam" = "team_abbr")) |> 
  # summarize(cor(motion_since_line_set_rate_overall, havoc_rate_overall))
  ggplot(aes(motion_since_line_set_rate_overall, havoc_rate_overall)) +
  geom_smooth(method = lm, se = FALSE,
              color = "gray",
              linetype = "dashed") +
  geom_point(alpha = 0.6, size = 3, aes(color = I(team_color))) +
  ggrepel::geom_text_repel(aes(label = passer_player_name), size = rel(2.5), seed = 2) +
  labs(x = "Motion rate across all passing plays",
       y = "Havoc rate across all passing plays")

# corr_motion_havoc |>
#   write_rds("figures/corr_motion_havoc.rds")

# shape rand eff posterior mean vs motion rate

corr_shape_motion <- plays_motion_rate_overall |> 
  inner_join(qb_shape_posterior) |> 
  filter(passer_player_id %in% qb_filtered$passer_player_id) |> 
  left_join(select(plays_havoc_rate_overall, passer_player_id, posteam)) |> 
  left_join(nflreadr::load_teams(), by = c("posteam" = "team_abbr")) |> 
  # summarize(cor(motion_since_line_set_rate_overall, posterior_mean_shape))
  ggplot(aes(posterior_mean_shape, motion_since_line_set_rate_overall)) +
  geom_smooth(method = lm, se = FALSE,
              color = "gray",
              linetype = "dashed") +
  geom_point(alpha = 0.6, size = 3, aes(color = I(team_color))) +
  ggrepel::geom_text_repel(aes(label = passer_player_name), size = rel(2.5), seed = 20) +
  labs(x = "Posterior mean for QB shape random effect",
       y = "Motion rate across all passing plays")

# corr_shape_motion |>
#   write_rds("figures/corr_shape_motion.rds")


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
# r = -0.07
corr_mean_shape <- qb_mean_posterior |> 
  left_join(qb_shape_posterior) |> 
  left_join(select(plays_havoc_rate_overall, passer_player_id, posteam)) |> 
  left_join(nflreadr::load_teams(), by = c("posteam" = "team_abbr")) |> 
  ggplot(aes(posterior_mean_shape, posterior_mean_mu)) +
  geom_smooth(method = lm, se = FALSE,
              color = "gray",
              linetype = "dashed") +
  geom_point(alpha = 0.6, size = 3, aes(color = I(team_color))) +
  ggrepel::geom_text_repel(aes(label = passer_player_name), size = rel(2.4), seed = 33) +
  labs(x = expression(paste("Posterior mean of ", u[q])),
       y = expression(paste("Posterior mean of ", b[q])))

# corr_mean_shape |>
#   write_rds("figures/corr_mean_shape.rds")


# look at receivers
receiver_filtered <- plays_snap_timing |> 
  distinct(gameId, playId, nflId) |> 
  count(nflId) |> 
  filter(n >= 20)

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
  scale_fill_manual(values = c("gold", "red", "purple", "gray")) +
  labs(x = "Player random effect",
       y = NULL,
       subtitle = "Players with min. 20 motions") +
  theme_minimal()

