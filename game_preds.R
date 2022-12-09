### World Cup 2022 Game Predictions
library(tidyverse)
options(dplyr.summarise.inform = F)
source('helpers.R')
library(gt)
library(gtExtras)

### Coefficients
posterior <- read_rds('model_objects/posterior.rds')
home_field <- mean(posterior$home_field)
neutral_field <- mean(posterior$neutral_field)
mu <- mean(posterior$mu)

### Read in Ratings and Schedule
df_ratings <- read_csv('predictions/ratings.csv')
schedule <- 
  read_csv('data/schedule.csv') %>% 
  mutate('date' = as.Date(date, '%m/%d/%y'))

### Expected Score for Each Game
preds  <- adorn_xg(schedule)

### WLD Probs
preds <- bind_cols(preds, map2_dfr(preds$lambda_1, preds$lambda_2, ~as_tibble(match_probs(.x,.y))))
preds_old <- read_csv('predictions/game_predictions.csv')
preds_old <- 
  preds_old %>% 
  filter(date < Sys.Date()) %>% 
  select(lambda_1, lambda_2, win, draw, loss)
preds[preds$date < Sys.Date(), c('lambda_1', 'lambda_2', 'win', 'draw', 'loss')] <- 
  preds_old

preds <- bind_cols(preds, map2_dfr(preds$lambda_1, preds$lambda_2, ~as_tibble(match_probs_ko(.x,.y))))
write_csv(preds, 'predictions/game_predictions.csv')


### Graphics
df_preds <- 
  preds %>% 
  filter(!is.na(group)) %>% 
  arrange(date) %>% 
  mutate('logo1' = paste0('flags/', team1, '.png')) %>% 
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>% 
  select(date, team1, logo1, team2, logo2, group, lambda_1, lambda_2, win, draw, loss)

# ### Graphics
df_preds_ko <-
  preds %>%
  filter(!is.na(ko_round)) %>%
  arrange(date) %>%
  mutate('logo1' = paste0('flags/', team1, '.png')) %>%
  mutate('logo2' = paste0('flags/', team2, '.png'))  %>%
  select(date, team1, logo1, team2, logo2, ko_round, lambda_1, lambda_2, win_ko, loss_ko)


t1 <-   
  df_preds %>% 
  slice(1:16) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**World Cup 2022 Game Predictions**'),
    subtitle = md('**Matchweek 1**')
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtsave_extra(t1, 'figures/matchweek1.png')

t2 <-   
  df_preds %>% 
  slice(17:32) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**World Cup 2022 Game Predictions**'),
    subtitle = md('**Matchweek 2**')
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtsave_extra(t2, 'figures/matchweek2.png')


t3 <-   
  df_preds %>% 
  slice(33:48) %>% 
  gt() %>% 
  cols_align('center') %>% 
  
  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
  fmt_percent(columns = c(win, loss, draw), decimals = 0, sep_mark = '') %>% 
  
  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds[, c('lambda_1', 'lambda_2')]))) %>% 
  data_color(columns = c(win, loss, draw),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>% 
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(group, lambda_2)
      )
    )
  ) %>% 
  
  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win', 'draw', 'loss')) %>% 
  
  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  text_transform(
    locations = cells_body(columns = "logo2"), 
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>% 
  
  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    group = 'Group',
    'lambda_1' = 'Team 1', 
    'lambda_2' = 'Team 2',
    'win' = 'Team 1',
    'draw' = 'Draw',
    'loss' = 'Team 2'
    
  ) %>% 
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = md('**World Cup 2022 Game Predictions**'),
    subtitle = md('**Matchweek 3**')
  ) %>% 
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )  

gtsave_extra(t3, 'figures/matchweek3.png')


t4 <-
  df_preds_ko %>%
  filter(str_detect(ko_round, 'R\\d+')) %>%
  select(-ko_round) %>%
  gt() %>%
  cols_align('center') %>%

  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
  fmt_percent(columns = c(win_ko, loss_ko), decimals = 0, sep_mark = '') %>%

  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>%
  data_color(columns = c(win_ko, loss_ko),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo2, lambda_2)
      )
    )
  ) %>%

  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win_ko', 'loss_ko')) %>%

  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  text_transform(
    locations = cells_body(columns = "logo2"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%

  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    'lambda_1' = 'Team 1',
    'lambda_2' = 'Team 2',
    'win_ko' = 'Team 1',
    'loss_ko' = 'Team 2'

  ) %>%
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = 'World Cup 2022 Game Predictions',
    subtitle = 'Round of 16'
  ) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtsave(t4, 'figures/r16_preds.png')

t5 <-
  df_preds_ko %>%
  filter(str_detect(ko_round, 'QF\\s+\\d+')) %>%
  select(-ko_round) %>%
  gt() %>%
  cols_align('center') %>%

  ### Round Numbers
  fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>%
  fmt_percent(columns = c(win_ko, loss_ko), decimals = 0, sep_mark = '') %>%

  ### Colors
  data_color(columns = c(lambda_1, lambda_2),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>%
  data_color(columns = c(win_ko, loss_ko),
             colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>%
  ### Borders
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_column_labels(
        columns = gt::everything()
      )
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(logo2, lambda_2)
      )
    )
  ) %>%

  tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>%
  tab_spanner(label = 'Match Outcome Probabilities', columns = c('win_ko', 'loss_ko')) %>%

  ### Logos
  text_transform(
    locations = cells_body(columns = "logo1"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%
  text_transform(
    locations = cells_body(columns = "logo2"),
    fn = function(x) map_chr(x, ~{
      local_image(filename =  as.character(.x), height = 30)
    })
  ) %>%

  ### Names
  cols_label(
    date = 'Date',
    team1 = 'Team 1',
    logo1 = '',
    team2 = 'Team 2',
    logo2 = '',
    'lambda_1' = 'Team 1',
    'lambda_2' = 'Team 2',
    'win_ko' = 'Team 1',
    'loss_ko' = 'Team 2'

  ) %>%
  tab_source_note("Luke Benz (@recspecs730)") %>%
  tab_header(
    title = 'World Cup 2022 Game Predictions',
    subtitle = 'Quarterfinals'
  ) %>%
  tab_options(column_labels.font.size = 20,
              heading.title.font.size = 40,
              heading.subtitle.font.size = 30,
              heading.title.font.weight = 'bold',
              heading.subtitle.font.weight = 'bold',
              column_labels.font.weight = 'bold'
  )

gtsave_extra(t5, 'figures/qf_preds.png')
# 
# t6 <-   
#   df_preds_ko %>% 
#   filter(str_detect(ko_round, 'SF\\s+\\d+')) %>% 
#   select(-ko_round) %>% 
#   gt() %>% 
#   cols_align('center') %>% 
#   
#   ### Round Numbers
#   fmt_number(columns = c(lambda_1, lambda_2), decimals = 2, sep_mark = '') %>% 
#   fmt_percent(columns = c(win_ko, loss_ko), decimals = 0, sep_mark = '') %>% 
#   
#   ### Colors
#   data_color(columns = c(lambda_1, lambda_2),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_preds_ko[, c('lambda_1', 'lambda_2')], na.rm = T))) %>% 
#   data_color(columns = c(win_ko, loss_ko),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
#   ### Borders
#   tab_style(
#     style = list(
#       cell_borders(
#         sides = "bottom",
#         color = "black",
#         weight = px(3)
#       )
#     ),
#     locations = list(
#       cells_column_labels(
#         columns = gt::everything()
#       )
#     )
#   ) %>% 
#   tab_style(
#     style = list(
#       cell_borders(
#         sides = "right",
#         color = "black",
#         weight = px(3)
#       )
#     ),
#     locations = list(
#       cells_body(
#         columns = c(logo2, lambda_2)
#       )
#     )
#   ) %>% 
#   
#   tab_spanner(label = 'Expected Goals', columns = c('lambda_1', 'lambda_2')) %>% 
#   tab_spanner(label = 'Match Outcome Probabilities', columns = c('win_ko', 'loss_ko')) %>% 
#   
#   ### Logos
#   text_transform(
#     locations = cells_body(columns = "logo1"), 
#     fn = function(x) map_chr(x, ~{
#       local_image(filename =  as.character(.x), height = 30)
#     })
#   ) %>% 
#   text_transform(
#     locations = cells_body(columns = "logo2"), 
#     fn = function(x) map_chr(x, ~{
#       local_image(filename =  as.character(.x), height = 30)
#     })
#   ) %>% 
#   
#   ### Names
#   cols_label(
#     date = 'Date',
#     team1 = 'Team 1',
#     logo1 = '',
#     team2 = 'Team 2',
#     logo2 = '',
#     'lambda_1' = 'Team 1', 
#     'lambda_2' = 'Team 2',
#     'win_ko' = 'Team 1',
#     'loss_ko' = 'Team 2'
#     
#   ) %>% 
#   tab_source_note("Luke Benz (@recspecs730)") %>%
#   tab_header(
#     title = 'Euro Cup 2021 Game Predictions',
#     subtitle = 'Semifinals'
#   ) %>% 
#   tab_options(column_labels.font.size = 20,
#               heading.title.font.size = 40,
#               heading.subtitle.font.size = 30,
#               heading.title.font.weight = 'bold',
#               heading.subtitle.font.weight = 'bold',
#               column_labels.font.weight = 'bold'
#   )  
# 
# gtsave(t6, 'figures/sf_preds.png')
# 
