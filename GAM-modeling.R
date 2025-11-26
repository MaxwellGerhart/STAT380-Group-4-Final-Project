# Remote Installing packages before
#install.packages("remotes")
#remotes::install_github("statsbomb/StatsBombR")

# Load libraries 
library(StatsBombR)
library(tidyverse)
library(mgcv)
library(pROC)

options(stringsAsFactors = FALSE)


#Loading Competitions + Selecting our Seasons
comps <- FreeCompetitions()

# Filter for 2015/2016 season and relevant leagues
target_comps <- comps %>% 
  filter(competition_name %in% c("Bundesliga", "Serie A", "Ligue 1", "La Liga"),
         season_name == "2015/2016")

#Loading matches
matches <- FreeMatches(target_comps)
head(matches)

#Loading events
events <- free_allevents(matches)
head(events)



#Extracting Shot Events
shots <- events %>%
  filter(type.name == "Shot") %>%
  mutate(
    x = map_dbl(location, 1),
    y = map_dbl(location, 2)
  ) %>%
  select(
    is_goal = shot.outcome.name,
    x,
    y,
    under_pressure,
    shot_body_part = shot.body_part.name,
    shot_technique = shot.technique.name,
    play_pattern = play_pattern.name
  ) %>%
  mutate(
    is_goal = ifelse(is_goal == "Goal", 1, 0),
    under_pressure = ifelse(is.na(under_pressure), 0, 1)
  )


#Creating Distance + Angle
goal_x <- 120
goal_y <- 40

shots <- shots %>%
  mutate(
    distance = sqrt((goal_x - x)^2 + (goal_y - y)^2),
    angle = abs(atan((y - goal_y) / (goal_x - x)))
  )


#Train/Test Split
set.seed(123)
idx <- sample(1:nrow(shots), 0.8 * nrow(shots))

train <- shots[idx, ]
test  <- shots[-idx, ]


#Fitting GAM Model
gam_model <- gam(
  is_goal ~ 
    s(distance) +
    s(angle) +
    under_pressure +
    shot_body_part +
    shot_technique +
    play_pattern,
  data = train,
  family = binomial
)


#Prediction + AUC
test$gam_xg <- predict(gam_model, newdata = test, type = "response")

auc_value <- roc(test$is_goal, test$gam_xg)$auc
auc_value


#GAM Plots
par(mfrow = c(1, 2))
plot(gam_model, select = 1, main = "Effect of Distance")
plot(gam_model, select = 2, main = "Effect of Angle")

