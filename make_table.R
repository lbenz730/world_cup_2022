library(tidyverse)
library(gt)
library(gtExtras)

df_stats <- 
  read_csv('predictions/ratings.csv') %>% select(team, alpha, delta, net_rating) %>% 
  inner_join(  read_csv('predictions/sim_results.csv')) %>% 
  arrange(desc(champ), desc(finals),
          desc(sf), desc(qf), desc(r16)) %>% 
  mutate('logo' = paste0('flags/', team, '.png')) %>% 
  select(team, logo, group, everything()) 

make_table <- function(Group = 'all', by_group = T) {
  if(Group == 'all') {
    df <- df_stats
    subtitle <- ''
    if(by_group) {
      df <- 
        df %>% 
        arrange(group) %>% 
        mutate('group' = paste('Group', group)) %>% 
        group_by(group) %>% 
        arrange(desc(r16), .by_group = T)
    }
  } else {
    df <- 
      df_stats %>% 
      filter(group == Group) %>% 
      arrange(desc(r16))
    subtitle <- paste('Group', Group)
  }
  
  df %>% 
    gt() %>% 
    
    ### Round Numbers
    fmt_number(columns = c(alpha, delta, net_rating, mean_pts, mean_gd), decimals = 2, sep_mark = '') %>% 
    fmt_percent(columns = c(r16, qf, sf, finals, champ), decimals = 0, sep_mark = '') %>% 
    
    ### Align Columns
    cols_align(align = "center") %>% 
    
    ### Colors
    data_color(columns = c(mean_pts),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 9))) %>% 
    data_color(columns = c(mean_gd),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$mean_gd))) %>% 
    data_color(columns = c(r16, qf, sf, finals, champ),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
    data_color(columns = c(alpha),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
    data_color(columns = c(net_rating),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
    data_color(columns = c(delta),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
    
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
          columns = c(group, net_rating, mean_gd)
        )
      )
    ) %>% 
    
    tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
    tab_spanner(label = 'Group Stage Averages', columns = c('mean_pts', 'mean_gd')) %>% 
    tab_spanner(label = 'Knockout Round', columns = c('r16', 'qf', 'sf',  'finals', 'champ')) %>% 
    
    ### Logos
    text_transform(
      locations = cells_body(columns = "logo"), 
      fn = function(x) map_chr(x, ~{
        local_image(filename =  as.character(.x), height = 30)
      })
    ) %>% 
    
    ### Names
    cols_label(
      team = '',
      logo = '',
      group = 'Group',
      alpha = 'Offense',
      delta = 'Defense',
      net_rating = 'Overall',
      mean_pts = 'Points',
      mean_gd = 'Goal Diff',
      r16 = 'R16',
      qf = 'QF',
      sf = 'SF',
      finals = 'Finals',
      champ = 'Champ'
      
    ) %>% 
    tab_source_note("Luke Benz (@recspecs730)") %>%
    tab_source_note("Ratings = Change in Log Goal Expectations") %>%
    tab_source_note("Based on 10,000 Simulations") %>%
    tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
    tab_header(
      title = md('**World Cup 2022**'),
      subtitle = md(paste0('**', subtitle, '**'))
    ) %>% 
    tab_options(column_labels.font.size = 20,
                row_group.font.weight = 'bold',
                row_group.font.size = 20,
                heading.title.font.size = 40,
                heading.subtitle.font.size = 30,
                heading.title.font.weight = 'bold',
                heading.subtitle.font.weight = 'bold',
                column_labels.font.weight = 'bold'
    )
}


table <- make_table('all')
gtsave_extra(table, filename = 'figures/world_cup_2022_bygroup.png')
table <- make_table('all', F)
gtsave_extra(table, filename = 'figures/world_cup_2022.png')
map(LETTERS[1:8], ~gtsave(make_table(Group = .x), filename = paste0('figures/', .x, '.png')))


# 
# round <- 'r16'
# rounds <- names(df_stats)[9:which(names(df_stats) == round)]
# 
# df <- 
#   df_stats[df_stats[[round]] > 0 & df_stats$champ > 0, ] %>% 
#   select(-mean_pts, -mean_gd, -any_of(rounds))
# 
# 
# 
# 
# 
# ko_table <-
#   df %>% 
#   gt() %>% 
#   
#   ### Round Numbers
#   fmt_number(columns = c(alpha, delta, net_rating), decimals = 2, sep_mark = '') %>% 
#   fmt_percent(columns = c(finals, champ), decimals = 0, sep_mark = '') %>% 
#   
#   ### Align Columns
#   cols_align(align = "center", columns = T) %>% 
#   
#   data_color(columns = c(finals, champ),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = c(0, 1))) %>% 
#   data_color(columns = c(alpha),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$alpha))) %>% 
#   data_color(columns = c(net_rating),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$net_rating))) %>% 
#   data_color(columns = c(delta),
#              colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100), domain = range(df_stats$delta), reverse = T)) %>% 
#   
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
#         columns = c(group, net_rating)
#       )
#     )
#   ) %>% 
#   
#   tab_spanner(label = 'Ratings', columns = c('alpha', 'delta', 'net_rating')) %>% 
#   tab_spanner(label = 'Knockout Round', columns = c('finals', 'champ')) %>% 
#   
#   ### Logos
#   text_transform(
#     locations = cells_body(columns = "logo"), 
#     fn = function(x) map_chr(x, ~{
#       local_image(filename =  as.character(.x), height = 30)
#     })
#   ) %>% 
#   
#   ### Names
#   cols_label(
#     team = '',
#     logo = '',
#     group = 'Group',
#     alpha = 'Offense',
#     delta = 'Defense',
#     net_rating = 'Overall',
#     r16 = 'R16',
#     qf = 'QF',
#     sf = 'SF',
#     finals = 'Finals',
#     champ = 'Champ'
#     
#   ) %>% 
#   tab_source_note("Luke Benz (@recspecs730)") %>%
#   tab_source_note("Ratings = Change in Log Goal Expectations") %>%
#   tab_source_note("Based on 10,000 Simulations") %>%
#   tab_source_note("Data: Kaggle | Country Images: Flaticon.com") %>%
#   tab_header(
#     title = md('World Cup 2022'),
#     subtitle = md('Knockout Round'),
#   ) %>% 
#   tab_options(column_labels.font.size = 20,
#               heading.title.font.size = 40,
#               heading.subtitle.font.size = 30,
#               heading.title.font.weight = 'bold',
#               heading.subtitle.font.weight = 'bold',
#               column_labels.font.weight = 'bold'
#   )
# 
# gtsave_extra(ko_table, filename = 'figures/knockout_wc_2022.png')
