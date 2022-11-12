### Dictionary of Team Codes
team_codes <- function(df) {
  teams <- sort(unique(c(df$home_team, df$away_team)))
  codes <- 1:length(teams)
  names(codes) <- teams
  
  return(codes)
}

### Generate Goal Expectations Given Teams + Location
goal_expectations <- function(team_1, team_2, location) {
  if(is.na(team_1)) {
    return(list('lambda_1' = NA,
                'lambda_2' = NA))
  }
  ### Team 1 Offensive/Defensive Ratings
  alpha_1 <- filter(df_ratings, team == team_1) %>% pull(alpha)
  delta_1 <- filter(df_ratings, team == team_1) %>% pull(delta)
  
  ### Team 2 Offensive/Defensive Ratings
  alpha_2 <- filter(df_ratings, team == team_2) %>% pull(alpha)
  delta_2 <- filter(df_ratings, team == team_2) %>% pull(delta) 
  
  ### Location Adjustments
  loc_1 <- case_when(team_1 == location ~ home_field,
                     team_2 == location ~ 0,
                     T ~ neutral_field)
  
  loc_2 <- case_when(team_1 == location ~ 0,
                     team_2 == location ~ home_field,
                     T ~ neutral_field)
  
  ### Goal Expectations
  lambda_1 <- exp(mu + alpha_1 + delta_2 + loc_1)
  lambda_2 <- exp(mu + alpha_2 + delta_1 + loc_1)
  
  return(list('lambda_1' = lambda_1, 
              'lambda_2' = lambda_2))
  
}

adorn_xg <- function(df) {
  df_xg <- pmap_dfr(list('team_1' = df$team1,
                         'team_2' = df$team2,
                         'location' = df$location),
                    ~{as_tibble(goal_expectations(..1, ..2, ..3))}) 
  return(bind_cols(df, df_xg))
}

### Simulate Group Stage
sim_group_stage <- function(df_group_stage) {
  ### Sim Each Game
  ix1 <- is.na(df_group_stage$team1_score)
  ix2 <- is.na(df_group_stage$team2_score)
  df_group_stage$team1_score[ix1] <- 
    rpois(sum(ix1), lambda = df_group_stage$lambda_1[ix1])
  df_group_stage$team2_score[ix2] <- 
    rpois(sum(ix2), lambda = df_group_stage$lambda_2[ix2])
  
  ### Aggregate Results
  df_results <- 
    bind_rows(
      select(df_group_stage, 'team' = team1, 'opp' = team2, 'team_score' = team1_score, 'opp_score' = team2_score, group),
      select(df_group_stage, 'team' = team2, 'opp' = team1, 'team_score' = team2_score, 'opp_score' = team1_score, group)
    )
  
  standings <- 
    df_results %>% 
    group_by(group, team) %>% 
    summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
              'goal_diff' = sum(team_score - opp_score),
              'goals_scored' = sum(team_score),
              'goals_allowed' = sum(opp_score)) %>% 
    ungroup() %>% 
    arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>% 
    group_by(group) %>% 
    mutate('place' = 1:n()) %>% 
    ungroup()
  
  ### Tiebreakers
  standings <- 
    group_tiebreak(standings, df_results) %>% 
    arrange(group, place) 
  
  ### Teams To Advance
  standings <- 
    standings %>% 
    mutate('progress' = case_when(place < 3 ~ T,
                                  T ~ F))
  
  return(standings)
  
}

group_tiebreak <- function(standings, df_results) {
  df_final <- NULL
  
  for(g in LETTERS[1:8]) {
    group_standings <- filter(standings, group == g)
    group_results <- filter(df_results, group == g)
    
    if(group_standings$points[1] != group_standings$points[4]) {
      if(group_standings$points[1] == group_standings$points[3]) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[1:3], opp %in% group_standings$team[1:3]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:3] <- ix
      } 
      if((group_standings$points[1] != group_standings$points[3]) & 
         (group_standings$points[1] == group_standings$points[2])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[1:2], opp %in% group_standings$team[1:2]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[1:2] <- ix
      } 
      if(group_standings$points[2] == group_standings$points[4]) {
        tiebreak_order <- 
          group_results %>% 
          filter(team %in% group_standings$team[2:4], opp %in% group_standings$team[2:4]) %>% 
          group_by(group, team) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored)) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:3, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:4] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[1] != group_standings$points[3]) &
         (group_standings$points[2] == group_standings$points[3])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[2:3], opp %in% group_standings$team[2:3]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[2:3] <- ix
      }
      if((group_standings$points[2] != group_standings$points[4]) &
         (group_standings$points[3] == group_standings$points[4])) {
        tiebreak_order <- 
          group_results %>% 
          inner_join(group_standings %>% select(team, place), by = 'team') %>% 
          filter(team %in% group_standings$team[3:4], opp %in% group_standings$team[3:4]) %>% 
          group_by(group, team, place) %>% 
          summarise('points' = 3 * sum(team_score > opp_score) + sum(team_score == opp_score),
                    'goal_diff' = sum(team_score - opp_score),
                    'goals_scored' = sum(team_score),
                    'goals_allowed' = sum(opp_score)) %>% 
          ungroup() %>% 
          arrange(group, desc(points), desc(goal_diff), desc(goals_scored), place) %>% 
          group_by(group) %>% 
          mutate('place' = 1:n()) %>% 
          ungroup() %>% 
          pull(team)
        
        ix <- map_dbl(1:2, ~which(group_standings$team == tiebreak_order[.x]))
        group_standings$place[3:4] <- ix
      }
    }
    df_final <- bind_rows(df_final, group_standings)
  }
  
  
  return(df_final)
}

build_knockout_bracket <- function(group_stage_results) {
  
  a <- filter(group_stage_results, group == 'A') %>% pull(team)
  b <- filter(group_stage_results, group == 'B') %>% pull(team)
  c <- filter(group_stage_results, group == 'C') %>% pull(team)
  d <- filter(group_stage_results, group == 'D') %>% pull(team)
  e <- filter(group_stage_results, group == 'E') %>% pull(team)
  f <- filter(group_stage_results, group == 'F') %>% pull(team)
  g <- filter(group_stage_results, group == 'G') %>% pull(team)
  h <- filter(group_stage_results, group == 'H') %>% pull(team)
 
  
  return(tibble('team1' = c(a[1], c[1], e[1], g[1], b[1], d[1], f[1], h[1]),
                'team2' = c(b[2], d[2], f[2], h[2], a[2], c[2], e[2], g[2])))
  
  
}

### Simulate KO Round Games
sim_ko_round <- function(df) {
  
  lambdas_1 <- df$lambda_1[is.na(df$team1_score)]
  lambdas_2 <- df$lambda_2[is.na(df$team2_score)]
  
  n <- length(lambdas_1)
  if(n > 0) {
    goals_1 <- rpois(n, lambdas_1)
    goals_2 <- rpois(n, lambdas_2)
    
    ### In event of tie, assume scoring rate of 1/3 for extra time
    tie_ix <- goals_1 == goals_2
    if(sum(tie_ix) > 0) {
      goals_1[tie_ix] <- goals_1[tie_ix] + rpois(sum(tie_ix), lambdas_1[tie_ix]/3)
      goals_2[tie_ix] <- goals_2[tie_ix] + rpois(sum(tie_ix), lambdas_2[tie_ix]/3)
      
      ## If Still Tied, Flip Coin For Shoot Out (increase/decrease goals_1 w/ p = 0.5)
      tie_ix <- goals_1 == goals_2
      if(sum(tie_ix) > 0) {
        goals_1[tie_ix] <- goals_1[tie_ix] + sample(c(0.1, -0.1), size = sum(tie_ix), replace = T)
      }
    }
    df$team1_score[is.na(df$team1_score)] <- goals_1
    df$team2_score[is.na(df$team2_score)] <- goals_2
  }  
  

  
  return(df)
}

### W/D/L given expectations
match_probs <- function(lambda_1, lambda_2) {
  max_goals <- 10
  score_matrix <- dpois(0:max_goals, lambda_1) %o% dpois(0:max_goals, lambda_2)
  tie_prob <- sum(diag(score_matrix))
  win_prob <- sum(score_matrix[lower.tri(score_matrix)])
  loss_prob <- sum(score_matrix[upper.tri(score_matrix)])
  return(list('win' = win_prob, 'draw' = tie_prob, 'loss' = loss_prob))
}


match_probs_ko <- function(lambda_1, lambda_2) {
  regulation <- match_probs(lambda_1, lambda_2)
  extra_time <- match_probs(lambda_1/3, lambda_2/3)
  
  win <- regulation$win + regulation$draw * extra_time$win + 0.5 * regulation$draw * extra_time$draw 
  loss <- regulation$loss + regulation$draw * extra_time$loss + 0.5 * regulation$draw * extra_time$draw 

  return(list('win_ko' = win, 'loss_ko' = loss))
}
