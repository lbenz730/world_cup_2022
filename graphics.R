library(tidyverse)
library(ggimage)
library(furrr)
options(future.fork.enable = T)
options(dplyr.summarise.inform = F)
plan(multicore(workers = parallel::detectCores()-1))

history <- 
  read_csv('predictions/history.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png')) %>% 
  mutate('eliminated' = (qf == 0))

df_stats <- 
  read_csv('predictions/sim_results.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png')) 

schedule <- 
  read_csv('data/schedule.csv')

df_r16 <- 
  schedule %>% 
  filter(grepl('R16', ko_round))
bracket1 <- c(df_r16$team1[1:4], df_r16$team2[1:4])
bracket2 <- c(df_r16$team1[1:4], df_r16$team2[1:4])
eliminated_teams <- history$team[history$date == max(history$date) & history$r16 == 0]

history_ko <- 
  history %>% 
  filter(!team %in% eliminated_teams)


theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 24, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  plot.subtitle = element_text(size = 20, hjust = 0.5),
                  strip.text = element_text(size = 14, hjust = 0.5),
                  legend.position = "none")
)

transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

ggplot(history, aes(x = date, y = r16)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(x = 'Date',
       y = 'Chances of Reaching Knockout Round',
       title = 'World Cup 2022',
       subtitle = 'Knockout Round Chances Over Time')
ggsave('figures/r16.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = qf)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Quarterfinals',
       title = 'World Cup 2022',
       subtitle = 'QF Chances Over Time')

ggsave('figures/qf.png', height = 12/1.2, width = 16/1.2)

ggplot(history_ko, aes(x = date, y = qf)) +
  facet_wrap(~ifelse(team %in% bracket1, 'Bracket 1', 'Bracket 2'), ncol = 2) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history_ko, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history_ko, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Quarterfinals',
       title = 'World Cup 2022',
       subtitle = 'QF Chances Over Time')

ggsave('figures/qf_by_bracket.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = sf)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Semifinals',
       title = 'World Cup 2022',
       subtitle = 'SF Chances Over Time')

ggsave('figures/sf.png', height = 12/1.2, width = 16/1.2)

ggplot(history_ko, aes(x = date, y = sf)) +
  facet_wrap(~ifelse(team %in% bracket1, 'Bracket 1', 'Bracket 2'), ncol = 2) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history_ko, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history_ko, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Semifinals',
       title = 'World Cup 2022',
       subtitle = 'SF Chances Over Time')

ggsave('figures/sf_by_bracket.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = finals)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Finals',
       title = 'World Cup 2022',
       subtitle = 'Finals Chances Over Time')

ggsave('figures/finals.png', height = 12/1.2, width = 16/1.2)

ggplot(history_ko, aes(x = date, y = finals)) +
  facet_wrap(~ifelse(team %in% bracket1, 'Bracket 1', 'Bracket 2'), ncol = 2) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history_ko, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history_ko, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Reaching Finals',
       title = 'World Cup 2022',
       subtitle = 'Finals Chances Over Time')

ggsave('figures/finals_by_bracket.png', height = 12/1.2, width = 16/1.2)


ggplot(history, aes(x = date, y = champ)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  labs(x = 'Date',
       y = 'Chances of Winning Tournament',
       title = 'World Cup 2022',
       subtitle = 'Title Chances Over Time')

ggsave('figures/champ.png', height = 12/1.2, width = 16/1.2)

ggplot(history_ko, aes(x = date, y = champ)) +
  facet_wrap(~ifelse(team %in% bracket1, 'Bracket 1', 'Bracket 2'), ncol = 2) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(data = filter(history_ko, !eliminated), 
             aes(image = logo),
             size = 0.085) + 
  geom_image(data = filter(history_ko, eliminated), 
             aes(image = logo),
             image_fun = transparent, 
             size = 0.085) + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Winning Tournament',
       title = 'World Cup 2022',
       subtitle = 'Title Chances Over Time')

ggsave('figures/champ_by_bracket.png', height = 12/1.2, width = 16/1.2)


df_elim <- 
  df_stats %>% 
  filter(r16 > 0) %>% 
  mutate('elim_Group' = 1 - r16,
         'elim_R16' = r16 - qf,
         'elim_QF' = qf - sf,
         'elim_SF' = sf - finals,
         'elim_Final' = finals - champ,
         'elim_Champ' = champ) %>% 
  mutate('expected_round' = elim_Group + 2 * elim_R16 + 3 * elim_QF + 4 * elim_SF + 5 * elim_Final + 6 * elim_Champ) %>% 
  pivot_longer(contains('elim_'),
               names_to = 'elim_round',
               names_prefix = 'elim_',
               values_to = 'elim_prob') %>% 
  mutate('elim_round' = factor(elim_round, levels = c('Group', 'R16', 'QF', 'SF', 'Final', 'Champ'))) %>% 
  mutate('team' = fct_reorder(team, desc(expected_round)))


labels <- paste0("<img src ='", unique(df_elim %>% arrange(-expected_round) %>% pull(logo)), "', width = '20'/>")

ggplot(df_elim, aes(x = team, y = elim_prob)) +
  geom_col(aes(fill = elim_round), position = position_fill(reverse = T)) + 
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = 'Probability of Elimination at Stage',
       x = '',
       title = 'FIFA World Cup 2022',
       subtitle = 'Elimination Snapshot',
       fill = 'Elimination Round') + 
  theme(axis.text.x = ggtext::element_markdown(),
        legend.position = 'bottom')

ggsave('figures/elim.png', height = 9/1.2, width = 16/1.2)


match3_plot <- function(group_, score_max, colors) {
  gsr <- read_rds('predictions/sim_rds/group_stage_game_results.rds')
  gsr_ <- read_rds('predictions/sim_rds/group_stage_results.rds')
  
  df_scores <-
    future_map_dfr(1:10000, ~{
      gsr[[.x]] %>% 
        filter(group == group_) %>%
        slice(5:6) %>%
        mutate('sim_id' = .x)
    })
  
  df_advance <-
    future_map_dfr(1:10000, ~{
      gsr_[[.x]] %>% 
        filter(group == group_) %>%
        mutate('sim_id' = .x)
    })
  
  
  df_summary <-
    df_scores %>%
    mutate('match' = case_when(team < opp ~ paste(team, '-', opp),
                               team > opp ~ paste(opp, '-', team))) %>%
    mutate('result' = case_when(team_score > opp_score ~ paste(team, team_score, '-', opp_score, opp),
                                team_score < opp_score ~ paste(opp, opp_score, '-', team_score, team),
                                team_score == opp_score & team < opp ~ paste(team, team_score, '-', opp_score, opp),
                                team_score == opp_score & team > opp ~ paste(opp, opp_score, '-', team_score, team))) %>%
    mutate('label' = case_when(team_score > opp_score ~ paste0("<img src = 'flags/", team, ".png', width = '12'/>", team_score, ' - ', opp_score,  "<img src = 'flags/", opp, ".png', width = '12'/>"),
                               team_score < opp_score ~ paste0("<img src = 'flags/", opp, ".png', width = '12'/>", opp_score, ' - ', team_score,  "<img src = 'flags/", team, ".png', width = '12'/>"),
                               team_score == opp_score & team < opp ~ paste0("<img src = 'flags/", team, ".png', width = '12'/>", team_score, ' - ', opp_score,  "<img src = 'flags/", opp, ".png', width = '12'/>"),
                               team_score == opp_score & team > opp ~ paste0("<img src = 'flags/", opp, ".png', width = '12'/>", opp_score, ' - ', team_score,  "<img src = 'flags/", team, ".png', width = '12'/>"))) %>%
    
    mutate('margin' = team_score - opp_score,
           'winning_score' = pmax(team_score, opp_score),
           'losing_score' = pmin(team_score, opp_score),) %>%
    select(-team, -opp) %>%
    left_join(df_advance) %>%
    group_by(match, team, result, margin, winning_score, losing_score, label) %>%
    summarise('n' = n(),
              'place_1' = mean(place == 1),
              'place_2' = mean(place == 2),
              'p_advance' = mean(progress)) %>%
    filter(winning_score <= score_max) %>%
    ungroup() %>%
    arrange(match, margin, -winning_score, losing_score) %>%
    mutate('result' = factor(result, levels = unique(result))) %>%
    mutate('label' = factor(label, levels = unique(label)))
  
  
  
  ggplot(df_summary, aes(x = label, y = p_advance, group = team)) +
    facet_wrap(~match, scales = 'free_x', nrow = 2) +
    geom_col(aes(fill = team), position = 'dodge') +
    geom_text(aes(label = paste0(round(100 * p_advance), '%')),
              size = 3,
              vjust = -0.5,
              position = position_dodge(width = 0.99)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = colors) +
    theme(axis.text.x = ggtext::element_markdown(),
          legend.position = 'bottom') +
    labs(title = 'Probability of Reaching Knockout Round',
         subtitle = paste('Group', group_),
         x = '',
         y = 'Probability of Reaching Knockout Round',
         fill = '')
  
  
  ggsave(paste0('figures/group_', group_, '_match3.png'), height = 9, width = 16)
}

# match3_plot('A', score_max = 2, c('#FFD100', '#FFA500', '#8A1538', '#00853F'))
# match3_plot('B', score_max = 2, c('#2B57AC', '#239F40', '#0A3161', '#C8102E'))
# match3_plot('C', score_max = 2, c('#6CACE4', '#006341', '#C8102E', '#165d31'))
# match3_plot('D', score_max = 2, c('#FFF200', '#C60C30', '#002654', '#C8102E'))
# match3_plot('E', score_max = 2, c('#002654', '#000000', '#BC002D', '#F1BF00'))
# match3_plot('F', score_max = 2, c('#FFCD00', '#D80621', '#012169', '#C1272D'))
# match3_plot('G', score_max = 2, c('#FEDD00', '#007A5E', '#0000FF', '#DA291C'))
# match3_plot('H', score_max = 2, c('#FFD100', '#046A38', '#CD2E3A', '#001489'))

