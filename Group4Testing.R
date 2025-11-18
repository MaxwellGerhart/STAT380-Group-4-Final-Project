#install.packages("devtools") 
#devtools::install_github("statsbomb/StatsBombR")

library(StatsBombR)
library(tidyverse) 

comps <- FreeCompetitions()

prem1516 <- comps %>%
  filter(competition_id == "2", season_id == "27")

laliga1516 <- comps %>%
  filter(competition_id == "11", season_id == "27")

bundes1516 <- comps %>%
  filter(competition_id == "9", season_id == "27")

ligue1516 <- comps %>%
  filter(competition_id == "7", season_id == "27")

seriea1516 <- comps %>%
  filter(competition_id == "12", season_id == "27")


# Fetch matches for each competition
prem1516_matches <- FreeMatches(prem1516)
laliga1516_matches <- FreeMatches(laliga1516)
bundes1516_matches <- FreeMatches(bundes1516)
ligue1516_matches <- FreeMatches(ligue1516)
seriea1516_matches <- FreeMatches(seriea1516)

# Combine all matches into one data frame, except premier league
matches_1516 <- bind_rows(
  laliga1516_matches,
  bundes1516_matches,
  ligue1516_matches,
  seriea1516_matches,
)

# Get all events from matches data frame
events <- free_allevents(MatchesDF = matches_1516, Parallel = TRUE)

# Filter for shots only
shots <- events %>%
  filter(type.name == "Shot") %>%
  select(
    player.name,
    team.id,
    location,
    under_pressure,
    play_pattern.id,
    starts_with("shot.")
  ) %>%
  mutate(is_goal = shot.outcome.name == "Goal") %>% 
  # Convert location to x and y coordinates
  mutate(
    location_x = map_dbl(location, 1),
    location_y = map_dbl(location, 2)
  ) %>%
  # Calculate angle to goal
  mutate(
    angle = atan2(7.32 * location_x, (16.5 - location_y) * location_x) * (180 / pi)
  )

# Fit a logistic regression model to predict goal probability
set.seed(380)
train_index <- sample(1:nrow(shots), size = 0.8 * nrow(shots))
train_data <- shots[train_index, ]
test_data <- shots[-train_index, ]

xg_model <- glm(
  is_goal ~ location_x * location_y + shot.body_part.name + shot.technique.name + under_pressure + play_pattern.id +
  angle,
  data = train_data,
  family = binomial(link = "logit")
)

# Summary of the model
summary(xg_model)

# Predict xG for each shot
predicted_xg <- predict(xg_model, newdata = test_data, type = "response")
predicted_goal <- ifelse(predicted_xg > 0.5, 1, 0)
mean(predicted_goal == test_data$is_goal)

# Prem shots
prem_shots <- free_allevents(MatchesDF = prem1516_matches, Parallel = TRUE) %>%
  filter(type.name == "Shot", competition_id == 2) %>%
  select(
    player.name,
    team.id,
    location,
    under_pressure,
    play_pattern.id,
    starts_with("shot.")
  ) %>%
  mutate(is_goal = shot.outcome.name == "Goal") %>% 
  mutate(
    location_x = map_dbl(location, 1),
    location_y = map_dbl(location, 2)
  ) %>%
  mutate(
    angle = atan2(7.32 * location_x, (16.5 - location_y) * location_x) * (180 / pi)
  )

# testing
predicted_xg <- predict(xg_model, newdata = prem_shots, type = "response")
predicted_goal <- ifelse(predicted_xg > 0.5, 1, 0)
mean(predicted_goal == prem_shots$is_goal)
