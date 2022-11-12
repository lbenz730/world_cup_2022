data {
  int<lower=1> num_clubs;                                     // number of clubs
  int<lower=1> num_games;                                     // number of games
  
  int<lower=1,upper=num_clubs> home_team_code[num_games];     // home club for game g
  int<lower=1,upper=num_clubs> away_team_code[num_games];     // away club for game g
  
  int<lower=0> h_goals[num_games];                            // home goals for game g
  int<lower=0> a_goals[num_games];                            // away goals for game g
  
  int<lower=0,upper=1> ind_home[num_games]; // indicator of home game
  
   int<lower=0> weights[num_games];      // weights for game importance
}
parameters {
  vector[num_clubs] alpha;                  // attacking intercepts
  vector[num_clubs] delta;                  // defending intercepts
  
  real<lower=0> sigma_a;                  // attacking sd
  real<lower=0> sigma_d;                  // defending sd
  
  real mu;                                 // mean goals/game
  real home_field;                        // home field advantage
  real neutral_field;                   // Neutral site game intercept
}
model {
  vector[num_games] lambda1;
  vector[num_games] lambda2;
  
  // priors
  alpha ~ normal(0, sigma_a);
  delta ~ normal(0, sigma_d);
  mu ~ normal(0, 5);
  sigma_a ~ inv_gamma(1,1);
  sigma_d ~ inv_gamma(1,1);
  home_field ~ normal(0, 5);
  neutral_field ~ normal(0, 5);
  
  // likelihood
  for (g in 1:num_games) {
    lambda1[g] = exp(mu + home_field * ind_home[g] + neutral_field * (1 - ind_home[g]) + alpha[home_team_code[g]] + delta[away_team_code[g]]);
    lambda2[g] = exp(mu + neutral_field * (1 - ind_home[g])  + alpha[away_team_code[g]] + delta[home_team_code[g]]);
  }

  for (g in 1:num_games) {
   target += poisson_lpmf(h_goals[g] | lambda1[g]) * weights[g];
   target += poisson_lpmf(a_goals[g] | lambda2[g]) * weights[g];
  }
}
