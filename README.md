# Explainable xG model paper

This repository consists the supplemental materials of the paper "Explainable expected goal models for performance analysis in football analytics". First 


## Data

We focus in our paper on 315,430 shots-related event data (containing 33,656 goals $\sim 10.66\%$ of total shots) from the 12,655 matches in 7 seasons between 2014-15 and 2020-21 from the top-five European football leagues which are Serie A, Bundesliga, La Liga, English Premier League, Ligue 1. The dataset is collected from [Understat](https://understat.com) by using the R-package [worldfootballR](https://github.com/JaseZiv/worldfootballR) and excluded the 1,012 shots resulting in own goals due to their unrelated pattern from the concept of the model. The following function is used for scraping the data from the leagues over 7 seasons:

```
ligue1_2020_shot_location <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2020)
ligue1_2019_shot_location <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2019)
...
ligue1_2014_shot_location <- understat_league_season_shots(league = "Ligue 1", season_start_year = 2014)
```

Then combine them as `dataset`:

```
dataset <- rbind(ligue1_2020_shot_location, ligue1_2019_shot_location, ...)
```

Do not forget that this steps takes a few hours depending on the processing power of your computer!


## Pre-processing of the raw dataset

This section introduces the dataset and how it is pre-processed. First data is imported from a .csv file is `raw_data`, then the features `distanceToGoal` and `angleToGoal` are extracted from the coordinated `X` and `Y`. The features `status`, `distanceToGoal`, `angleToGoal`, `h_a`, `shotType`, `lastAction`, `minute`, `league`, and `season` are prepared for analysis and modeling.

```
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


## Modeling

We use the forester [forester](https://github.com/ModelOriented/forester) AutoML tool to train various tree-based classification models from [XGBoost](https://github.com/dmlc/xgboost), [randomForest](https://github.com/ranger/ranger), [LightGBM](https://github.com/microsoft/LightGBM), and [CatBoost](https://github.com/catboost/catboost) libraries. These models do not provide any pre-processing steps like missing data imputation, encoding, or transformation and show quite good performance in the presence of outlier(s) in the dataset which is used to train models. We use the train-test split (80-20) to train and validate the models. Moreover, another advantage of the forester is that provides an easy connection to [DALEX](https://github.com/ModelOriented/DALEX) model explanation and exploration ecosystem.

We changed and expanded some functions of the `forester` package. You can see the reasons for this action below:

* The `forester` returns the predicted labels, we changed this with predicted probabilities to calculate the performance metrics which are based on probabilities such as log-loss, Brier score and MCC. 
* The `forester` returns only the output of the best performing model in terms of the value of intended metric, we expanded it to return the output of all models for comparing their performance with the additional metrics.
* After under-sample the dataset, the `ranger` changes the reference class in the model and causes a inconsistency. Thus, we add an argument to the `make_ranger` and `forester` functions to control the reference class. 

You can find the R codes of modeling part in [Rcodes.rmd](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Rcodes.Rmd).


## Figures

### Figure 1: The distribution of angle to goal and distance to goal of shots regarding goal status in the last seven seasons of top-five European football leagues

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

<img src="https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/shot_dist.png" width="800">


### Figure 3: The aggregated xG profiles of Schalke 04 and Bayern Munich for angle and distance to goal in the match is played on Jan 24, 2021

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

# creating aggregated profiles for the shots of Schalke 04 and Bayern Munich for "distanceToGoal" feature
ap_dtg_schalke <- ingredients::aggregate_profiles(cp_schalke,
                                                  variables = "distanceToGoal",
                                                  type = "partial")
ap_dtg_bayern  <- ingredients::aggregate_profiles(cp_bayern,
                                                  variables = "distanceToGoal",
                                                  type = "partial")

# drawing plot of APs for "distanceToGoal" feature
plot(ap_dtg_schalke, ap_dtg_bayern)
```

<img src="https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/dtg.png" width="600">


```{r}
# creating aggregated profiles for the shots of Schalke 04 and Bayern Munich for "angleToGoal" feature
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


### Figure 4: The End-of-season statistics of Burak Yilmaz (BY), Lionel Messi (LM), and Robert Lewandowski (RL) in the season of 2020-21

```{r}
# subsetting the shots of Burak Yilmaz (by), Lionel Messi (lm) and Robert Lewandowski (rl) in the season of 2020-21
selected_observations_by <- shot_stats %>%
  filter(player_id == 8637, season == 2020)
selected_observations_lm <- shot_stats %>%
  filter(player_id == 2097, season == 2020)
selected_observations_rl <- shot_stats %>%
  filter(player_id == 227, season == 2020)

# creating ceteris-paribus profiles for the shots of Burak Yilmaz, Lionel Messi, and Robert Lewandowski
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

# creating aggregated profiles for the shots of Burak Yilmaz, Lionel Messi, and Robert Lewandowski for "distanceToGoal" feature
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
# creating aggregated profiles for the shots of Burak Yilmaz, Lionel Messi, and Robert Lewandowski for "angleToGoal" feature
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

### Figure 5: The behavior comparison of the random forest models trained on original, over-sampled, and under-sampled data in terms of PDP curves

```{r}
# creating an explainer for the random forest model on the original dataset
org_rf_explainer <- explain(model = original_model$model3,
                            data = original_model$test_data[,-1],
                            y = original_model$test_data$status,
                            type = "classification",
                            label = "original data")

# creating an explainer for the random forest model on the under-sampled dataset
under_rf_explainer <- explain(model = under_model$model3,
                              data = under_model$test_data[,-1],
                              y = under_model$test_data$status,
                              type = "classification",
                              label = "under-sampled")

# creating an explainer for the random forest model on the over-sampled dataset
over_rf_explainer <- explain(model = over_model$model3,
                             data = over_model$test_data[,-1],
                             y = over_model$test_data$status,
                             type = "classification",
                             label = "over-sampled")

# drawing the comparison plot of PDPs for "angleToGoal" feature
  plot(model_profile(org_rf_explainer, variables = "angleToGoal"),
     model_profile(under_rf_explainer, variables = "angleToGoal"),
     model_profile(over_rf_explainer,  variables = "angleToGoal"))
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/angle.png)

```{r}
# drawing the comparison plot of PDPs for "distanceToGoal" feature
plot(DALEX::model_profile(org_rf_explainer,   variables = "distanceToGoal"),
     DALEX::model_profile(under_rf_explainer, variables = "distanceToGoal"),
     DALEX::model_profile(over_rf_explainer,  variables = "distanceToGoal"))
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/distance.png)

```{r}
# drawing the comparison plot of PDPs for "minute" feature
plot(DALEX::model_profile(org_rf_explainer,   variables = "minute"),
     DALEX::model_profile(under_rf_explainer, variables = "minute"),
     DALEX::model_profile(over_rf_explainer,  variables = "minute"))
```

![](https://github.com/mcavs/Explainable_xG_model_paper/blob/main/Plots/minute.png)
