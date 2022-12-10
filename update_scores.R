library(XML)
library(RCurl)
library(tidyverse)

get_scores <- function(date) {
  date_ <- gsub('-', '', date)
  url <- paste0('https://www.espn.com/soccer/fixtures/_/date/', date_, '/league/fifa.world')
  scores <- readHTMLTable(getURL(url))
  scores <- scores[[1]]
  penalties_ix <- 1 + which(str_detect(scores$result, 'FT-Pens'))
  penalties_winners <- gsub('\\s+advance.*', '', scores$match[penalties_ix])
  df <- 
    tibble('date' = as.Date(date_, '%Y%m%d'),
           'team1' = gsub('\\s*[A-Z][A-Z][A-Z]v\\s*', '', gsub( '\\s[A-Z]+\\d+.*$', '', scores[,1])),
           'team2' = gsub( '\\s[A-Z]+$', '', scores[,2]),
           'team1_score' = as.numeric(str_extract(scores[,1], '\\d+') ),
           'team2_score' = as.numeric(str_extract(scores[,1], '\\d+$') ),
           'shootout_winner' = NA) %>% 
    slice(setdiff(1:nrow(.), penalties_ix))
  
  if(length(penalties_ix) > 0) {
    df$shootout_winner[penalties_ix] <- penalties_winners
  }
  
  df <- bind_rows(df, select(df, date,
                             'team2' = team1, 'team1' = team2, 
                             'team1_score' = team2_score, 'team2_score' = team1_score, 
                             shootout_winner))
  
  return(df)
  
}

### Read In Schedule
schedule <- 
  read_csv('data/schedule.csv') %>% 
  mutate('date' = as.Date(date, '%m/%d/%y'))

### Get Scores for Tournament
scores <- map_dfr(seq.Date(as.Date('2022-11-20'), Sys.Date(), 1), get_scores)
ko_games <- 
  map_dfr(schedule$date[!is.na(schedule$ko_round)], get_scores) %>% 
  filter(team1 %in% c(scores$team1, scores$team2),  team2 %in% c(scores$team1, scores$team2)) %>% 
  distinct() %>% 
  filter(team1 > team2)

schedule$team1[!is.na(schedule$ko_round) & is.na(schedule$team1_score)] <- NA
schedule$team2[!is.na(schedule$ko_round) & is.na(schedule$team2_score)] <- NA

for(i in 1:nrow(ko_games)) {
  ix_game <- min(which(schedule$date == ko_games$date[i] & is.na(schedule$team1)))
  schedule$team1[ix_game] <- ko_games$team1[i]
  schedule$team2[ix_game] <- ko_games$team2[i]
}

### Update Scores
schedule <- 
  schedule %>% 
  select(-contains('score'),
         -contains('shootout_winner')) %>% 
  left_join(scores, by = c("date", "team1", "team2"))

### Save Results
write_csv(schedule, 'data/schedule.csv')

