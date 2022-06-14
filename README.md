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


## Figure 3: The aggregated xG profiles of Schalke 04 and Bayern Munich for angle and distance to goal in the match is played on Jan 24, 2021

```{r}
# subsetting the shots of Schalke 04 and Bayern Munich
selected_observations_schalke <- shot_stats %>%
  filter(h_a == "h", match_id == 15298)
selected_observations_bayern <- shot_stats %>%
  filter(h_a == "a", match_id == 15298)

# creating ceteris-paribus profiles for the shots of Schalke 04 and Bayern Munich
cp_schalke <- ingredients::ceteris_paribus(over_model$model3,
                                           selected_observations_schalke)
cp_bayern  <- ingredients::ceteris_paribus(over_model$model3,
                                           selected_observations_bayern)

# re-labeling the profiles
cp_schalke$`_label_` <- "Schalke 04"
cp_bayern$`_label_`  <- "Bayern Munich"

# creating aggregated profiles for the shots of Schalke 04 and Bayern Munich
# for "distanceToGoal" feature
ap_dtg_schalke <- ingredients::aggregate_profiles(cp_schalke,
                                                  variables = "distanceToGoal",
                                                  type = "partial")
ap_dtg_bayern  <- ingredients::aggregate_profiles(cp_bayern,
                                                  variables = "distanceToGoal",
                                                  type = "partial")

# drawing plot of APs for "distanceToGoal" feature
plot(ap_dtg_schalke, ap_dtg_bayern)
```

![Figure: The distribution of angle to goal and distance to goal of shots regarding goal status in the last seven seasons of top-five European football leagues](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/dtg.png)

```{r}
# creating aggregated profiles for the shots of Schalke 04 and Bayern Munich
# for "angleToGoal" feature
ap_atg_schalke <- ingredients::aggregate_profiles(cp_schalke,
                                                  variables = "angleToGoal",
                                                  type = "partial")
ap_atg_bayern  <- ingredients::aggregate_profiles(cp_bayern,
                                                  variables = "angleToGoal",
                                                  type = "partial")

# drawing the comparison plot of APs for "angleToGoal" feature
plot(ap_atg_schalke, ap_atg_bayern)
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/atg.png)


## Figure 4: The End-of-season statistics of Burak Yilmaz (BY), Lionel Messi (LM), and Robert Lewandowski (RL) in the season of 2020-21

```{r}
# subsetting the shots of Burak Yilmaz (by), Lionel Messi (lm) 
# and Robert Lewandowski (rl) in the season of 2020-21
selected_observations_by <- shot_stats %>%
  filter(player_id == 8637, season == 2020)
selected_observations_lm <- shot_stats %>%
  filter(player_id == 2097, season == 2020)
selected_observations_rl <- shot_stats %>%
  filter(player_id == 227, season == 2020)

# creating ceteris-paribus profiles for the shots of 
# Burak Yilmaz, Lionel Messi, and Robert Lewandowski
cp_by <- ceteris_paribus(over_model$model3,
                         selected_observations_by)
cp_lm <- ceteris_paribus(over_model$model3,
                         selected_observations_lm)
cp_rl <- ceteris_paribus(over_model$model3,
                         selected_observations_rl)

# re-labeling the profiles
cp_by$`_label_` <- "Burak Yilmaz"
cp_lm$`_label_` <- "Lionel Messi"
cp_rl$`_label_` <- "Robert Lewandowski"

# creating aggregated profiles for the shots of 
# Burak Yilmaz, Lionel Messi, and Robert Lewandowski
# for "distanceToGoal" feature
ap_dtg_by <- aggregate_profiles(cp_by,
                                variables = "distanceToGoal",
                                type = "partial")
ap_dtg_lm <- aggregate_profiles(cp_lm,
                                variables = "distanceToGoal",
                                type = "partial")
ap_dtg_rl <- aggregate_profiles(cp_rl,
                                variables = "distanceToGoal",
                                type = "partial")

# drawing the comparison plot of APs for "distanceToGoal" feature
plot(ap_dtg_by, ap_dtg_lm, ap_dtg_rl)
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/player_dtg.png)

```{r}
# creating aggregated profiles for the shots of 
# Burak Yilmaz, Lionel Messi, and Robert Lewandowski
# for "angleToGoal" feature
ap_atg_by <- aggregate_profiles(cp_by,
                                variables = "angleToGoal",
                                type = "partial")
ap_atg_lm <- aggregate_profiles(cp_lm,
                                variables = "angleToGoal",
                                type = "partial")
ap_atg_rl <- aggregate_profiles(cp_rl,
                                variables = "angleToGoal",
                                type = "partial")

# drawing the comparison plot of APs for "angleToGoal" feature
plot(ap_atg_by, ap_atg_lm, ap_atg_rl)
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/player_atg.png)
