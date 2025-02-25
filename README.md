# timing

Repository for the paper *A multilevel model with heterogeneous variances for snap timing in the National Football League* (formerly Big Data Bowl entry *Down, set, hut! Explaining variability in snap timing on plays with motion*)

## Abstract

Player tracking data have provided great opportunities to generate novel insights into understudied areas of American football, such as pre-snap motion. Using a Bayesian multilevel model with heterogeneous variances, we provide an assessment of NFL quarterbacks and their ability to synchronize the timing of the ball snap with pre-snap movement from their teammates. We focus on passing plays with receivers in motion at the snap and running a route, and define the snap timing as the time between the moment a receiver begins motioning and the ball snap event. We assume a Gamma distribution for the play-level snap timing and model the mean parameter with player and team random effects, along with relevant fixed effects such as the motion type identified via a Gaussian mixture model. Most importantly, we model the shape parameter with quarterback random effects, which enables us to estimate the differences in snap timing variability among NFL quarterbacks. We demonstrate that higher variability in snap timing is beneficial for the passing game, as it relates to facing less havoc created by the opposing defense. We also obtain a quarterback leaderboard based on our snap timing variability measure, and Patrick Mahomes stands out as the top player.

## Related links

* [arXiv preprint](https://arxiv.org/abs/2502.16313)

* [NFL Big Data Bowl 2025 submission](https://www.kaggle.com/code/tindata/down-set-hut)

* Download data from Kaggle [here](https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/data) and store the `.csv` files in a `data` subdirectory.
