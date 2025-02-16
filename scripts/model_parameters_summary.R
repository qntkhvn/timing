library(tidyverse)
library(brms)

snap_timing_fit <- read_rds("data/snap_timing_fit.rds")

snap_timing_fit

library(tidybayes)

parameters_summary <- snap_timing_fit |> 
  tidy_draws() |> 
  summarise_draws()


parameters_summary |> 
  filter(str_detect(variable, "sd_"))

# fixed effects table
summary(snap_timing_fit)$fixed

summary(snap_timing_fit)$random
