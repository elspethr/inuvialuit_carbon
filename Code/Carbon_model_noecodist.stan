data{
  //# IHS data
  int N;                   // N harvests reported
  int N_took;              // Hunts in Toonik dataset
  int Nsp;                 // Species
  int Necotype;            // Species groups 
  int sim;                 // Simulated or real data
  array[N] int EW_miss;    // Missing harvest data
  array[N] real harv_meas; // Observed report
  array[N] int species;    // Species of that report
  array[N] int ecotype;    // Species group of that report
  array[N] int markettype; // Assignment to market replacements
  array[N] int community;  // Where?

  // "Priors" for modeling realistic measurement error in IHS data
  array[N] real log_meanharv_eco;  // Mean harvest size for each "ecotype"
  array[N] real log_sdharv_eco;    // SD harvest size for each "ecotype"
  real scale_factor_error;         // Scale of error distribution
  array[6] int tripsperc;          // Trips per community

  // Tooniktoyok data
  array[N_took] real Fuel;        
  array[N_took] real EW;

  // Conversion factors
  array[N] real spEW;              // Edible weight data (village-specific for char)
  array[N] real market_scalar;
  array[N] real scalar_barge_low;
  array[N] real scalar_barge_high;
  array[N] real scalar_mail_low;
  array[N] real scalar_mail_high;
  real fuel_cost;
  array[N] real scalar_fuel_emissions_low;
  array[N] real scalar_fuel_emissions_high;
  array[6,2] real scalar_fuel_emissions_villages;
}

transformed data{
  array[N] real log_HM;
  
  array[N_took] real log_Fuel;
  array[N_took] int zero_EW;
  array[N_took] real log_EW;
  
  log_HM = log(harv_meas);
  
  log_Fuel = log(Fuel);

  for (i in 1:N_took) {
    zero_EW[i] = EW[i] == 0 ? 1:0;
    log_EW[i] = EW[i] == 0 ? -99999 : log(EW[i]);
  }
}

parameters{
  array[N] real log_harv_true;      // Estimated "true" harvest 

  real theta;                       // Zero infl fact
  vector[2] alpha;                  // Parameters for positive EW
  real<lower=0> phi_alpha;          // SD fuel dists

  real beta;                        // No EW
  real<lower=0> phi_beta;           // SD fuel dists
}

model{
  // Priors
  theta ~ normal(-2, 2.5);
  alpha ~ normal(0, 2.5);
  beta ~ normal(0, 2.5);

  phi_alpha ~ exponential(1);
  phi_beta ~ exponential(1);

  // Fuel model
  zero_EW ~ bernoulli_logit(theta);

  for(i in 1:N_took){
    if(EW[i] != 0){
      log_Fuel[i] ~ normal(alpha[1] + alpha[2] * log_EW[i], phi_alpha);
    }
    else{ 
      log_Fuel[i] ~ normal(beta, phi_beta);
    }
  }

  // Deheaping model
  for(i in 1:N){
   if(EW_miss[i]!=0){
     log_harv_true[i] ~ normal(log_meanharv_eco[i], log_sdharv_eco[i]); 
   }
    if(EW_miss[i]==0){
      log_HM[i] ~ normal(log_harv_true[i], scale_factor_error);
    }
  }   
}

generated quantities{
 array[N] real harvest_number;
 array[N] real harvest_weight;
 array[N] real fuel_usage;

 array[N] real market_costs;

 array[N] real emissions_barge_low;
 array[N] real emissions_barge_high;
 array[N] real emissions_mail_low;
 array[N] real emissions_mail_high;
 
 array[N] real fuel_costs;
 array[N] real fuel_emissions_low;
 array[N] real fuel_emissions_high;

 array[6] real N_failures;
 array[6] real fuel_usage_failures;
 array[6] real fuel_failures_costs;
 array[6] real fuel_failures_low_emissions;
 array[6] real fuel_failures_high_emissions;

 real fuel_usage_in;
 real fuel_usage_out;
 real fuel_usage_all;
 real total_harvest_weight;

 // IHS, in-sample
 for (i in 1:N) {
   harvest_number[i] = exp(log_harv_true[i]);
   harvest_weight[i] = harvest_number[i] * spEW[i];
   market_costs[i] = harvest_weight[i] * market_scalar[i];
   emissions_barge_low[i] = harvest_weight[i] * scalar_barge_low[i];
   emissions_barge_high[i] = harvest_weight[i] * scalar_barge_high[i];
   emissions_mail_low[i] = harvest_weight[i] * scalar_mail_low[i];
   emissions_mail_high[i] = harvest_weight[i] * scalar_mail_high[i];
   fuel_usage[i] = exp(normal_rng(alpha[1] + alpha[2] * log(harvest_weight[i]), phi_alpha));
   fuel_costs[i] = fuel_usage[i] * fuel_cost;
   fuel_emissions_low[i] = fuel_usage[i] * scalar_fuel_emissions_low[i];
   fuel_emissions_high[i] = fuel_usage[i] * scalar_fuel_emissions_high[i];
 }

 fuel_usage_in = sum(fuel_usage);
 
 // IHS, out-of-sample
 for (k in 1:6) {
  int j = 0;
  N_failures[k] = (inv_logit(theta)/(1-inv_logit(theta)))*tripsperc[k];
  fuel_usage_failures[k] = 0;
  while(j < N_failures[k]){
    j += 1;
    fuel_usage_failures[k] += exp(normal_rng(beta, phi_beta));
  }
  fuel_failures_costs[k] = fuel_usage_failures[k] * fuel_cost;
  fuel_failures_low_emissions[k] = fuel_usage_failures[k] * scalar_fuel_emissions_villages[k,1];
  fuel_failures_high_emissions[k] = fuel_usage_failures[k] * scalar_fuel_emissions_villages[k,2];
 }

 fuel_usage_out = sum(fuel_usage_failures);

 fuel_usage_all = fuel_usage_in + fuel_usage_out;

 total_harvest_weight = sum(harvest_weight);
}
