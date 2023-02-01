data{
  //IHS data
  int N; // N harvests reported
  int Nsp; // # species
  int Necotype; // # species groups 
  array[N] int EW_miss; // missing harvest data
  array[N] real harv_meas; // observed report
  array[N] int species; // species of that report
  array[N] int ecotype; // species group of that report
  array[N] int markettype; // assignment to market replacements
  array[N] int community; // where?
  //priors for modeling realistic measurement error in IHS data
  array[Necotype] real log_meanharv_eco; // mean harvest size for each "ecotype"
  array[Necotype] real log_sdharv_eco; // sd harvest size for each "ecotype"
  real scale_factor_error; // scale of error distribution
  array[6] int tripsperc;
  //Tooniktoyok data
  array[118] real Fuel;
  array[118] real EW;
  //conversion factors
  array[Nsp+6] real spEW; // edible weight data (village-specific for char)
  array[6,3] real market_costs;
  array[6,4,3] real market_emissions;
  real fuel_cost;
  array[6,2] real fuel_emissions;
}
transformed data{
  array[118] real log_Fuel;
  array[118] int zero;
  log_Fuel = log(Fuel);
  for (i in 1:118) {
    zero[i] = EW[i] == 0 ? 1:0;
  }
}
parameters{
  array[N] real log_harv_true; // estimated "true" harvest
  real b; // slope EW
  array[2] real<lower=0> phi; // sd fuel dists
  real a; // intercept for positive EW
  real a2; // intercept no EW
  real<lower=0,upper=1> theta;  // zinfl fact
}
model{
  vector[118] mu;
  b ~ normal(0, 1);
  a ~ normal(0, 1);
  phi ~ exponential(1);
  a2 ~ normal(0, 1);
  theta ~ beta(2, 2);
  // fuel model
  for (i in 1:118) {
    zero[i] ~ bernoulli(theta);
    if (EW[i] != 0) {
      mu[i] = a + b * log(EW[i]);
      log_Fuel[i] ~ normal(mu[i], phi[1]);
    }
    else { 
      log_Fuel[i] ~ normal(a2, phi[2]);
    }
  }
  // deheaping
  for (i in 1:N) {
    if (EW_miss[i]==0) {
      // p. report | true value
      harv_meas[i] ~ lognormal(log_harv_true[i], scale_factor_error); 
      // p. true value | type distribution
      log_harv_true[i] ~ normal(log_meanharv_eco[ecotype[i]], log_sdharv_eco[ecotype[i]]); 
    }
    else {
      log_harv_true[i] ~ normal(log_meanharv_eco[ecotype[i]], log_sdharv_eco[ecotype[i]]); 
    } 
  }   
}
generated quantities{
  array[N] real harv_true;
  array[N] real sim_harv;
  array[N] real EW_est;
  array[N] real fuel_est;
  array[7,4] real harvest_est;
  array[7,4] real market_cost_est;
  array[7,4,4] real carbon_cost_est;
  array[7] real fuel_obs_est;
  array[7] real fuel_zero_est;
  array[7] real fuel_total;
  array[7] real fuel_total_cost;
  array[7, 2] real fuel_total_emissions;
  array[7] real nzero;
  {
    int j;
    harvest_est = rep_array(0.0, 7, 4);
    market_cost_est = rep_array(0.0, 7, 4);
    carbon_cost_est = rep_array(0.0, 7, 4, 4);
    fuel_obs_est = rep_array(0.0, 7);
    fuel_zero_est  = rep_array(0.0, 7);
    // go through i, simulate out lognormal with mu and sigma of the species
    for (i in 1:N) {
      harv_true[i] = exp(log_harv_true[i]);
      sim_harv[i] = lognormal_rng(log_harv_true[i], scale_factor_error);
      if (species[i] == 12 || species[i] == 13) {
        int z = community[i];
        EW_est[i] = sim_harv[i]*spEW[53+z];
      }
      else {
        EW_est[i] = sim_harv[i]*spEW[species[i]];
      }
      fuel_est[i] = exp(normal_rng(a + b*log(EW_est[i]), phi[1]));
      harvest_est[7,4] += EW_est[i];
      for (x in 1:6) {
        if (community[i] == x) {
            fuel_obs_est[x] += fuel_est[i];
          for (y in 1:3) {
            if (markettype[i] == y) {
              harvest_est[x,y] += EW_est[i];
              harvest_est[7,y] += EW_est[i];
              harvest_est[x,4] += EW_est[i];
              market_cost_est[x,y] += EW_est[i]*market_costs[x,y];
              market_cost_est[7,y] += EW_est[i]*market_costs[x,y];
              market_cost_est[x,4] += EW_est[i]*market_costs[x,y];
              market_cost_est[7,4] += EW_est[i]*market_costs[x,y];
              for (s in 1:4) {
                // note reindexing, got lazy :/
                carbon_cost_est[x,y,s] += EW_est[i]*market_emissions[x,s,y];
                carbon_cost_est[7,y,s] += EW_est[i]*market_emissions[x,s,y];
                carbon_cost_est[x,4,s] += EW_est[i]*market_emissions[x,s,y];
                carbon_cost_est[7,4,s] += EW_est[i]*market_emissions[x,s,y];
              }
            }
          }
        }
      }
    }
  // calcs for total weight, cost, carbon, fuel and fuel emitted
  for (x in 1:6) {
    j = 0;
    while (j < tripsperc[x]*(theta/(1-theta))) {
      fuel_zero_est[x] += exp(normal_rng(a2, phi[2]));
      nzero[x] = j;
      j += 1;
      fuel_total[x] = fuel_obs_est[x] + fuel_zero_est[x];
      fuel_total_cost[x] = fuel_total[x]*fuel_cost;
      fuel_total_emissions[x,1] = fuel_total[x]*fuel_emissions[x,1];
      fuel_total_emissions[x,2] = fuel_total[x]*fuel_emissions[x,2];
    }
  }
  nzero[7] = sum(nzero[1:6]);
  fuel_total[7] = sum(fuel_total[1:6]);
  fuel_total_cost[7] = sum(fuel_total_cost[1:6]);
  fuel_total_emissions[7,1] = sum(fuel_total_emissions[1:6,1]);
  fuel_total_emissions[7,2] = sum(fuel_total_emissions[1:6,2]);
  }
}
