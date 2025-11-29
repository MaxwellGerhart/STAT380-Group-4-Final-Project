# Load libraries
library(StatsBombR)
library(tidyverse)
library(randomForest)
library(pROC)

options(stringsAsFactors = FALSE)

#Loading Competitions + Selecting our Seasons
comps <- FreeCompetitions()

target_comps <- comps %>%
  filter(competition_name %in% c("Bundesliga", "Serie A", "Ligue 1", "La Liga"),
         season_name == "2015/2016")

# Loading the matches and events
matches <- FreeMatches(target_comps)
events  <- free_allevents(matches)

# Extraction of shot data
shots <- events %>%
  filter(type.name == "Shot") %>%
  mutate(
    x = map_dbl(location, 1),
    y = map_dbl(location, 2)
  ) %>%
  select(
    is_goal = shot.outcome.name,
    x, y,
    under_pressure,
    shot_body_part = shot.body_part.name,
    shot_technique = shot.technique.name,
    play_pattern = play_pattern.name
  ) %>%
  mutate(
    is_goal = ifelse(is_goal == "Goal", 1, 0),
    under_pressure = ifelse(is.na(under_pressure), 0, 1)
  )

# Create distance and angle features
goal_x <- 120
goal_y <- 40

shots <- shots %>%
  mutate(
    distance = sqrt((goal_x - x)^2 + (goal_y - y)^2),
    angle = abs(atan((y - goal_y) / (goal_x - x)))
  )

#Train/test split
set.seed(123)
idx <- sample(1:nrow(shots), size = 0.8 * nrow(shots))

train <- shots[idx, ]
test  <- shots[-idx, ]

# Random Forest requires a factor target
train$is_goal_factor <- factor(train$is_goal, levels = c(0, 1))

# Fiting the Random Forest model
set.seed(123)
rf_model <- randomForest(
  is_goal_factor ~ distance + angle + under_pressure +
    shot_body_part + shot_technique + play_pattern,
  data = train,
  ntree = 500,
  mtry = 3
)

#Create our predict on test data and extract the 
probs <- predict(rf_model, test, type = "prob")
rf_pred <- probs[,2]


# Compute AUC to analyze 
rf_auc <- auc(roc(test$is_goal, rf_pred))
rf_auc

# 9. Save model
saveRDS(rf_model, "xg_model_rf.rds")
