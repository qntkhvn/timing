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
# snap_timing_fit |> 
#   plot()

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

snap_timing_fit_no_cluster <- brm(
  bf(
    # mean
    frame_between ~
      factor(down) + play_clock_at_motion + factor(posteam_timeouts_remaining) + 
      position + n_motion_since_line_set + 
      # factor(routeRan) + y_change +
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

snap_timing_fit_no_mean_re <- brm(
  bf(
    # mean
    frame_between ~
      factor(down) + play_clock_at_motion + factor(posteam_timeouts_remaining) + 
      position + n_motion_since_line_set + factor(cluster),
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

# validation
# WAIC

waic_final <- waic(snap_timing_fit)
waic_no_cluster <- waic(snap_timing_fit_no_cluster)
waic_no_mean_re <- waic(snap_timing_fit_no_mean_re)

# We also explore a model with no cluster labels
# We validate to confirm that model with cluster label is better

waic_final
waic_no_cluster
waic_no_mean_re

# This confirms the needs for including random effects when modeling the mean snap timing, 
# as well as performing clustering to obtain labels for motion type.


# loo

loo_final <- loo(snap_timing_fit)
loo_no_cluster <- loo(snap_timing_fit_no_cluster)
loo_no_mean_re <- loo(snap_timing_fit_no_mean_re)

loo_final
loo_no_cluster
loo_no_mean_re


# leave one week out


snap_timing_lowo <- function(w){
  train <- filter(plays_snap_timing, week != w)
  test <- filter(plays_snap_timing, week == w)
  
  fit <- brm(
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
    iter = 5000,
    warmup = 2500,
    seed = 3,
    cores = 4,
    backend = "cmdstanr",
    data = train
  )

    out <- tibble(
      pred = predict(fit, newdata = test, cores = 4, allow_new_levels = TRUE)[,1],
      obs = pull(test, frame_between),
      week = w
    )
    return(out)
} 


snap_timing_lowo_results <- purrr::map(1:9, snap_timing_lowo) |>
  list_rbind()


snap_timing_lowo_reduced <- function(w){
  train <- filter(plays_snap_timing, week != w)
  test <- filter(plays_snap_timing, week == w)
  
  fit <- brm(
    frame_between ~ factor(down) + play_clock_at_motion + factor(posteam_timeouts_remaining) + 
      position + n_motion_since_line_set + factor(cluster),
    family = Gamma(link = "log"),
    chains = 4,
    iter = 5000,
    warmup = 2500,
    seed = 3,
    cores = 4,
    backend = "cmdstanr",
    data = train
  )
  
  out <- tibble(
    pred = predict(fit, newdata = test, cores = 4, allow_new_levels = TRUE)[,1],
    obs = pull(test, frame_between),
    week = w
  )
  return(out)
} 

snap_timing_lowo_reduced_results <- purrr::map(1:9, snap_timing_lowo_reduced) |>
  list_rbind()


# RMSE

library(yardstick)
snap_timing_full_rmse <- snap_timing_lowo_results |>
  group_by(week) |>
  rmse(obs, pred)

snap_timing_reduced_rmse <- snap_timing_lowo_reduced_results |>
  group_by(week) |>
  rmse(obs, pred)

mutate(snap_timing_full_rmse, ran_eff = "Yes") |>
  bind_rows(mutate(snap_timing_reduced_rmse, ran_eff = "No")) |>
  ggplot(aes(week, .estimate, color = ran_eff)) +
  geom_point() +
  geom_line() +
  labs(x = "Left-out week",
       y = "RMSE",
       color = "Random effects?") +
  scale_x_continuous(breaks = 1:9) +
  theme_light()


snap_timing_lowo_results |>
  mutate(resid = obs - pred,
         sq_err = resid ^ 2) |>
  group_by(week) |>
  summarize(mse = mean(sq_err, na.rm = TRUE),
            se = sd(sq_err, na.rm = TRUE) / sqrt(n()),
            type = "Yes") |>
  bind_rows(
    snap_timing_lowo_reduced_results |>
      mutate(resid = obs - pred,
             sq_err = resid ^ 2) |>
      group_by(week) |>
      summarize(mse = mean(sq_err, na.rm = TRUE),
                se = sd(sq_err, na.rm = TRUE) / sqrt(n()),
                type = "No")
  ) |>
  mutate(lower = mse - 2*se, upper = mse + 2*se) |> 
  ggplot(aes(week, mse, color = type)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.5), width = 0.5) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Left-out week",
       y = "MSE",
       color = "Random effects?") +
  theme_light() +
  theme(panel.grid.minor = element_blank())

mean(snap_timing_full_rmse$.estimate)
mean(snap_timing_reduced_rmse$.estimate)


snap_timing_lowo_results |> 
  mutate(resid = obs - pred) |> 
  summarize(mse = mean(resid ^ 2)) |> 
  as.data.frame()


snap_timing_lowo_reduced_results |> 
  mutate(resid = obs - pred) |> 
  summarize(mse = mean(resid ^ 2)) |> 
  as.data.frame()
