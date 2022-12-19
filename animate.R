library(tidyverse)
library(ggimage)
library(gganimate)

theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 24, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  plot.subtitle = element_text(size = 20, hjust = 0.5),
                  strip.text = element_text(size = 14, hjust = 0.5),
                  legend.position = "bottom")
)

history <- 
  read_csv('predictions/history.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png')) %>% 
  group_by(team) %>% 
  mutate('eliminated' = r16 == 0 | (date > as.Date('2022-12-03') & finals == 0)) %>% 
  mutate('elim_date' = min(date[eliminated])) %>% 
  ungroup() %>% 
  filter(date <= elim_date) %>% 
  # mutate('round' = case_when(date <= '2022-12-02' ~ 'Group Stage',
  #                            date <= '2022-12-06' ~ 'R16',
  #                            date <= '2022-12-10' ~ 'QF',
  #                            date <= '2022-12-14' ~ 'SF',
  #                            T ~ 'Finals')) %>% 
  mutate('round' = case_when(r16 == 1 & qf < 1 ~ 'R16',
                             qf == 1 & sf < 1 ~ 'QF',
                             sf == 1 & finals < 1 ~ 'SF',
                             finals == 1 & champ < 1 ~ 'Finals',
                             T ~ 'Group Stage')) %>% 
  mutate('round' = factor(round, levels = c('Group Stage', 'R16', 'QF', 'SF', 'Finals')))
           

p <- 
  ggplot(history, aes(x = date, y = champ)) +
  facet_wrap(~paste('Group', group), ncol = 4) +
  geom_line(aes(group = team, col = round), alpha = 0.8, lwd = 1.2, lineend = 'round') +
  geom_image(data = history, 
             aes(image = logo,),
             size = 0.125) +
  scale_y_continuous(limits = c(0,0.6), labels = scales::percent) + 
  scale_x_date(limits = as.Date(c('2022-11-18', '2022-12-16'))) + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'Date',
       y = 'Chances of Winning World Cup',
       title = 'FIFA World Cup 2022 Title Chances',
       subtitle = '{frame_along}',
       color = '',
       caption = 'Luke Benz (@recspecs730)') + 
  transition_reveal(date) + 
  ease_aes('linear')



animate(p, fps = 5, end_pause = 50, height = 5, width = 7, units = 'in', res = 150)
anim_save('figures/finals_animation.gif')
