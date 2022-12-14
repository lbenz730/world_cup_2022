### Update Scores
source('update_scores.R')

## Re-Fit Model After Each Round --> new preds
if(as.character(Sys.Date()) %in% c('2022-11-24', '2022-11-28', '2022-12-02', 
                                   '2022-12-06', '2022-12-10', '2022-12-14')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
source('run_sim.R')

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')

