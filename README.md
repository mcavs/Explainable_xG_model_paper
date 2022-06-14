# Explainable xG model paper

This repository consists the supplemental materials of the paper "Explainable expected goal models for performance analysis in football analytics" to reproduce the results given in.


## Changes made on {forester}

* 1
* 2
* 3
* 4

## Pre-processing of the raw dataset

This section introduces the dataset and how it is pre-processed. First data is imported from a .csv file is `raw_data`, then the features `distanceToGoal` and `angleToGoal` are extracted from the coordinated `X` and `Y`. The features `status`, `distanceToGoal`, `angleToGoal`, `h_a`, `shotType`, `lastAction`, `minute`, `league`, and `season` are prepared for analysis and modeling.

```
dataset <- read.csv("raw_data.csv")

shot_stats <- dataset %>% filter(result != "OwnGoal") %>%
  mutate(status = ifelse(result == "Goal", 1, 0)) %>%
  mutate(distanceToGoal = sqrt((105 - (X * 105)) ^ 2 + (32.5 - (Y * 68)) ^ 2)) %>%
  mutate(angleToGoal = abs(atan((7.32 * (105 - (X * 105))) / ((105 - (X * 105))^2 + (32.5 - (Y * 68)) ^ 2 - (7.32 / 2) ^ 2)) * 180 / pi)) %>%
  mutate(h_a = factor(h_a),
         situation = factor(situation),
         shotType = factor(shotType),
         lastAction = factor(lastAction),
         minute = as.numeric(minute)) %>%
  select(status, minute, h_a, situation, shotType, lastAction, 
         distanceToGoal, angleToGoal, league, season, match_id, result, player_id)
```

## Figure: The distribution of angle to goal and distance to goal of shots regarding goal status in the last seven seasons of top-five European football leagues

```
shot_vis <- data.frame(sta = as.factor(rep(shot_stats$status, 2)),
                       obs = c(shot_stats$distanceToGoal, 
                               shot_stats$angleToGoal),
                       vty = as.factor(c(rep("Distance to goal", length(shot_stats$distanceToGoal)),
                              rep("Angle to goal", length(shot_stats$angleToGoal)))),
                       lea = rep(shot_stats$league, 2),
                       sea = rep(shot_stats$season, 2))

shot_vis$lea[shot_vis$lea == "La_liga"] <- "La Liga"
shot_vis$lea[shot_vis$lea == "Ligue_1"] <- "Ligue 1"
shot_vis$lea[shot_vis$lea == "Serie_A"] <- "Serie A"

ggplot(shot_vis, 
       aes(x = obs, 
           group = sta, 
           fill = sta)) + 
  geom_density(alpha = 0.5) + 
  theme_bw() + 
  scale_fill_brewer(palette = "Set1",
                    name = "Goal Status", 
                    labels = c("No goal", "Goal")) + 
  theme(legend.position = "bottom") +
  labs(x = "Value (meter or angle)",
       y = "Density") + 
  facet_grid(lea ~ vty, 
             scales="free_x") 
```

![Figure: The distribution of angle to goal and distance to goal of shots regarding goal status in the last seven seasons of top-five European football leagues](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/shot_dist.png)
