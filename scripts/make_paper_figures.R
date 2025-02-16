plays_snap_timing <- read_rds("data/plays_snap_timing.rds")
snap_timing_fit <- read_rds("data/snap_timing_fit.rds")

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
posteriors_slab_paper <- snap_timing_fit |> 
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
       color = "Credible Interval") +
  theme_minimal(base_family = "Fira Sans") +
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_line(color = "gray95"),
        text = element_text(color = "black"),
        plot.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", size = rel(1.1), color = "black"),
        axis.title.x = element_text(face = "bold"),
        legend.position = "bottom",
        legend.margin = margin(t = -3))

ggsave(
  "figures/posteriors_slab_paper.png",
  plot = posteriors_slab_paper,
  width = 5,
  height = 7,
  bg = "white"
)


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
                           family = "Fira Sans", size = rel(3.8), seed = 31) +
  facet_wrap(~ play_subset) +
  labs(x = "Posterior mean for QB shape random effect",
       y = "Havoc rate") +
  theme_light() +
  annotate("segment", x = 0.115, y = 0.528, xend = 0.33, yend = 0.528, linewidth = 0.7,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  annotate("text", x = 0.33, y = 0.55, hjust = 1, lineheight = 1,
           label = "Higher variability\nin snap timing",
           family = "Fira Sans", fontface = "bold", size = rel(4)) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(family = "Fira Sans"),
        axis.title = element_text(face = "bold", size = rel(1.5)),
        strip.text = element_text(face = "bold", size = rel(1.2), color = "black"),
        strip.background = element_rect(fill = "gray90", color = NA),
        plot.title = element_text(hjust = 0.5))

ggsave(
  "figures/corr_havoc_paper.png",
  plot = corr_havoc,
  width = 10,
  height = 4.1
)
