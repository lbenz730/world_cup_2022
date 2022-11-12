library(tidyverse)
library(ggimage)

history <- 
  read_csv('predictions/history.csv') %>% 
  mutate('logo' = paste0('flags/', team, '.png'))

theme_set(theme_bw() + 
            theme(plot.title = element_text(size = 24, hjust = 0.5),
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12),
                  plot.subtitle = element_text(size = 20, hjust = 0.5),
                  strip.text = element_text(size = 14, hjust = 0.5),
                  legend.position = "none")
)

ggplot(history, aes(x = date, y = r16)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Knockout Round',
       title = 'World Cup 2022',
       subtitle = 'Knockout Round Chances Over Time')

ggsave('figures/r16.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = qf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Quarterfinals',
       title = 'World Cup 2021',
       subtitle = 'QF Chances Over Time')

ggsave('figures/qf.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = sf)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Semifinals',
       title = 'World Cup 2021',
       subtitle = 'SF Chances Over Time')

ggsave('figures/sf.png', height = 12/1.2, width = 16/1.2)

ggplot(history, aes(x = date, y = finals)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Reaching Finals',
       title = 'World Cup 2021',
       subtitle = 'Finals Chances Over Time')

ggsave('figures/finals.png', height = 12/1.2, width = 16/1.2)


ggplot(history, aes(x = date, y = champ)) +
  facet_wrap(~paste('Group', group)) +
  geom_line(aes(group = team), col = 'black', alpha = 0.4) +
  geom_image(aes(image = logo), size = 0.065) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x = 'Date',
       y = 'Chances of Winning Tournament',
       title = 'World Cup 2021',
       subtitle = 'Title Chances Over Time')

ggsave('figures/champ.png', height = 12/1.2, width = 16/1.2)
