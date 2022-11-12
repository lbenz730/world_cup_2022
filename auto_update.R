### Update Scores
source('update_scores.R')

### Re-Fit Model After Each Round --> new preds
if(as.character(Sys.Date()) %in% c('2022-11-25')) {
  source('fit_model.R')
  source('game_preds.R')
}

### Run Simulations
source('run_sim.R')

### Make Tables
source('make_table.R')

### Make Graphics
source('graphics.R')

