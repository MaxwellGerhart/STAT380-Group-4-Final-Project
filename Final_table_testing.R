library(StatsBombR)
library(tidyverse)
library(gt)

## 1. Get Premier League 2015–16 events ----
comps <- FreeCompetitions()
prem1516 <- comps %>%
  filter(competition_id == 2, season_id == 27)
prem1516_matches <- FreeMatches(prem1516)
prem_events <- free_allevents(
  MatchesDF = prem1516_matches,
  Parallel  = TRUE
) %>%
  allclean()

## 2. Load your xG model and get xG for each shot ----
xg_model_logistic <- readRDS("xg_model_logistic.rds")
shots <- prem_events %>%
  filter(type.name == "Shot") %>%
  mutate(is_goal = shot.outcome.name == "Goal") %>% 
  mutate(
    location_x = map_dbl(location, 1),
    location_y = map_dbl(location, 2)
  ) %>%
  mutate(
    angle = atan2(7.32 * location_x, (16.5 - location_y) * location_x) * (180 / pi)
  ) %>%
  mutate(
    # uses whatever predictors your model was trained on
    xg = predict(xg_model_logistic, newdata = ., type = "response")
  ) %>%
  filter(!is.na(xg)) %>%
  select(match_id, team.name, xg)

## 3. Simulation function for one match ----
simulate_match_from_xg <- function(match_shots, n_sims = 10000) {
  # expects: match_shots has columns team.name, xg
  
  xg   <- match_shots$xg
  team <- match_shots$team.name
  teams <- unique(team)
  
  # edge cases: no shots or not exactly two teams
  if (length(xg) == 0L || length(teams) != 2L) {
    return(tibble(
      team.name   = teams,
      win_prob    = 0,
      draw_prob   = 1,
      loss_prob   = 0,
      exp_points  = 1
    ))
  }
  
  idx1 <- which(team == teams[1])
  idx2 <- which(team == teams[2])
  
  n_shots <- length(xg)
  
  # prob matrix (n_sims x n_shots)
  prob_mat <- matrix(rep(xg, each = n_sims),
                     nrow = n_sims, ncol = n_shots, byrow = FALSE)
  
  # simulate goals per shot (Bernoulli)
  goal_mat <- matrix(
    rbinom(n_sims * n_shots, size = 1, prob = as.vector(prob_mat)),
    nrow = n_sims, ncol = n_shots
  )
  
  # goals per team per simulation
  goals_team1 <- rowSums(goal_mat[, idx1, drop = FALSE])
  goals_team2 <- rowSums(goal_mat[, idx2, drop = FALSE])
  
  team1_wins <- goals_team1 > goals_team2
  team2_wins <- goals_team2 > goals_team1
  draws      <- goals_team1 == goals_team2
  
  team1_win_prob <- mean(team1_wins)
  team2_win_prob <- mean(team2_wins)
  draw_prob      <- mean(draws)
  
  team1_exp_pts <- 3 * team1_win_prob + 1 * draw_prob
  team2_exp_pts <- 3 * team2_win_prob + 1 * draw_prob
  
  tibble(
    team.name  = teams,
    win_prob   = c(team1_win_prob, team2_win_prob),
    draw_prob  = draw_prob,
    loss_prob  = c(mean(!team1_wins & !draws),
                   mean(!team2_wins & !draws)),
    exp_points = c(team1_exp_pts, team2_exp_pts)
  )
}

## 4. Expected points per match and per season ----
# per match (two rows per match: one per team)
set.seed(380)

match_expected_points <- shots %>%
  group_by(match_id) %>%
  nest(data = -match_id) %>%
  mutate(
    sim_results = map(data, ~ simulate_match_from_xg(.x, n_sims = 10000))
  ) %>%
  select(match_id, sim_results) %>%
  unnest(sim_results)

# season totals for expected points
season_expected_points <- match_expected_points %>%
  group_by(team.name) %>%
  summarise(
    exp_points_season = sum(exp_points),
    .groups = "drop"
  )

## 5. Calculate xG and xGA ----
xg_summary <- shots %>%
  group_by(team.name) %>%
  summarise(
    xG = sum(xg),
    .groups = "drop"
  )

# xGA (xG conceded) - need to get opponent's xG for each match
xga_summary <- shots %>%
  left_join(
    prem1516_matches %>% 
      select(match_id, home_team.home_team_name, away_team.away_team_name),
    by = "match_id"
  ) %>%
  mutate(
    opponent = if_else(team.name == home_team.home_team_name, 
                       away_team.away_team_name, 
                       home_team.home_team_name)
  ) %>%
  group_by(opponent) %>%
  summarise(
    xGA = sum(xg),
    .groups = "drop"
  ) %>%
  rename(team.name = opponent)

## 6. Get actual results ----
actual_results <- prem1516_matches %>%
  # Home team results
  transmute(
    team.name = home_team.home_team_name,
    W = if_else(home_score > away_score, 1, 0),
    D = if_else(home_score == away_score, 1, 0),
    L = if_else(home_score < away_score, 1, 0),
    GF = home_score,
    GA = away_score,
    points = case_when(
      home_score > away_score ~ 3,
      home_score == away_score ~ 1,
      TRUE ~ 0
    )
  ) %>%
  bind_rows(
    # Away team results
    prem1516_matches %>%
      transmute(
        team.name = away_team.away_team_name,
        W = if_else(away_score > home_score, 1, 0),
        D = if_else(away_score == home_score, 1, 0),
        L = if_else(away_score < home_score, 1, 0),
        GF = away_score,
        GA = home_score,
        points = case_when(
          away_score > home_score ~ 3,
          away_score == home_score ~ 1,
          TRUE ~ 0
        )
      )
  ) %>%
  group_by(team.name) %>%
  summarise(
    W = sum(W),
    D = sum(D),
    L = sum(L),
    GF = sum(GF),
    GA = sum(GA),
    PTS = sum(points),
    .groups = "drop"
  )

## 7. Combine everything into final table ----
final_table <- actual_results %>%
  left_join(xg_summary, by = "team.name") %>%
  left_join(xga_summary, by = "team.name") %>%
  left_join(season_expected_points, by = "team.name") %>%
  rename(xPTS = exp_points_season) %>%
  arrange(desc(PTS)) %>%
  select(team.name, W, D, L, GF, GA, PTS, xG, xGA, xPTS)

print(final_table)

## 8. Create beautiful table ----
final_table %>%
  mutate(
    Rank = row_number(),
    .before = team.name
  ) %>%
  gt() %>%
  tab_header(
    title = "Premier League 2015-16 Season",
    subtitle = "Actual vs Expected Performance"
  ) %>%
  cols_label(
    Rank = "",
    team.name = "Team",
    W = "W",
    D = "D",
    L = "L",
    GF = "GF",
    GA = "GA",
    PTS = "PTS",
    xG = "xG",
    xGA = "xGA",
    xPTS = "xPTS"
  ) %>%
  fmt_number(
    columns = c(xG, xGA, xPTS),
    decimals = 1
  ) %>%
  data_color(
    columns = PTS,
    palette = "viridis",
    domain = NULL
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#f0f0f0"),
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = Rank <= 4
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#fff3cd"),
    locations = cells_body(
      rows = Rank == 5
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#f8d7da"),
    locations = cells_body(
      rows = Rank >= 18
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 20,
    heading.subtitle.font.size = 14,
    column_labels.font.weight = "bold"
  ) %>%
  cols_align(
    align = "center",
    columns = c(W, D, L, GF, GA, PTS, xG, xGA, xPTS)
  ) %>%
  cols_align(
    align = "left",
    columns = team.name
  )

