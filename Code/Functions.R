###########################################################################################
### determine scaling factor for deheaping ###
###########################################################################################
calc_sd = function(median, sigma) {
  # lognormal sigma
  # log(median) is lognormal mu
  return(sqrt((exp(sigma^2) - 1) * exp((2 * log(median)) + sigma^2)))
}

###########################################################################################
### format a dataframe for inspect ###
###########################################################################################
round_df = function(df, digits) {
  nums = vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] = round(df[,nums], digits = digits)
  return(df)
}

###########################################################################################
### inverse logit for binomial regression ###
###########################################################################################
inv_logit = function(x){
  p = 1/(1 + exp(-x))
  p = ifelse(x == Inf, 1, p)
  return(p)
}

###########################################################################################
### rounding to nearest integer ###
########################################################################################### 
n_int_digits = function(x) {
  result = floor(log10(abs(x)))
  result[!is.finite(result)] = 0
  return(result)
}

###########################################################################################
### parse data ###
###########################################################################################
to_long_format = function(IHS, Toonik){
 IHS_sim_successes = IHS[which(IHS$trip_successful==1),]
 IHS_sim_failures = IHS[which(IHS$trip_successful==0),]

 # (organize all model input data into long format)
 IHS_sim_successes$species_id = as.numeric(factor(IHS_sim_successes$species, levels=unique(animals$species)))
 IHS_sim_successes$ecotype_id = as.numeric(factor(animals$ecotype[IHS_sim_successes$species_id], levels=unique(animals$ecotype)))

 IHS_sim_successes$log_meanharv_eco = animals$log_harvest_per_encounter_mu[IHS_sim_successes$ecotype_id]
 IHS_sim_successes$log_sdharv_eco = animals$log_harvest_per_encounter_sd[IHS_sim_successes$ecotype_id]

 vill_id = as.numeric(factor(IHS_sim_successes$village))
 markettype_id = as.numeric(factor(animals$markettype[IHS_sim_successes$species_id]))

 for (i in 1:length(IHS_sim_successes$species)){
  if (IHS_sim_successes$species[i] %in% c("Char - Arctic", "Char - Dolly Varden")) {
    IHS_sim_successes$spEW[i] = c(0.65, 0.65, 1.6, 0.7, 1, 1.65)[vill_id[i]]
  }
  else {
    IHS_sim_successes$spEW[i] = animals$edible_weight[IHS_sim_successes$species_id[i]]
  }

  IHS_sim_successes$mc[i] = market_costs[vill_id[i], markettype_id[i]]
  IHS_sim_successes$barge_low[i] = market_emissions[vill_id[i], 1, markettype_id[i]]
  IHS_sim_successes$barge_high[i] = market_emissions[vill_id[i], 2, markettype_id[i]]
  IHS_sim_successes$mail_low[i] = market_emissions[vill_id[i], 3, markettype_id[i]]
  IHS_sim_successes$mail_high[i] = market_emissions[vill_id[i], 4, markettype_id[i]]
  IHS_sim_successes$fuel_emissions_lo[i] = rail_emissions_litre[vill_id[i],2]
  IHS_sim_successes$fuel_emissions_high[i] = rail_emissions_litre[vill_id[i],3]
  }

 scalar_fuel_emissions_villages = rail_emissions_litre[,2:3]

 N = length(IHS_sim_successes$species)

 x = 0.1 # scaling factor
 dat = list(EW = Toonik$reported_edible_weight,
            Fuel = Toonik$fuel_used,
            community = vill_id,
            harv_meas = IHS_sim_successes$reported_harvest,
            EW_miss = rep(0, N), #MISSING DATA NOT YET IMPLEMENTED IN SIM
            species = IHS_sim_successes$species_id,
            ecotype = IHS_sim_successes$ecotype_id,
            markettype = markettype_id,
            N = N,
            N_took = length(Toonik$reported_edible_weight),
            log_meanharv_eco = IHS_sim_successes$log_meanharv_eco,
            log_sdharv_eco = IHS_sim_successes$log_sdharv_eco,
            Nsp = length(unique(IHS_sim_successes$species_id)),
            tripsperc = as.numeric(table(vill_id)),
            Necotype = length(unique(IHS_sim_successes$ecotype_id)),
            spEW = IHS_sim_successes$spEW,
            scale_factor_error=x, 
            # cost and carbon scenarios
            market_scalar = IHS_sim_successes$mc,
            scalar_barge_low = IHS_sim_successes$barge_low,
            scalar_barge_high = IHS_sim_successes$barge_high,
            scalar_mail_low = IHS_sim_successes$mail_low,
            scalar_mail_high = IHS_sim_successes$mail_high,
            fuel_cost = 1.76,
            scalar_fuel_emissions_low = IHS_sim_successes$fuel_emissions_lo,
            scalar_fuel_emissions_high = IHS_sim_successes$fuel_emissions_high,
            scalar_fuel_emissions_villages = rail_emissions_litre[,2:3],
            sim=1)
  return(dat)
}

###########################################################################################
### create animals ###
###########################################################################################
make_animals = function(){
 animals = data.frame(species=NA, ecotype=NA, village=NA, markettype=NA, 
                     edible_weight=NA, log_harvest_per_encounter_mu=NA,
                     log_harvest_per_encounter_sd=NA) 

 animals[1,] = c("Caribou - Barren Ground", "Caribou", "Any","Mammal", 36.5, 0.7, 0.7)
 animals[2,] = c("Seal - Ringed", "Seal - Ringed", "Any","Mammal", 13.0, 0.9, 0.9)
 animals[3,] = c("Hare - Arctic", "Hare", "Any","Mammal", 2.9, 1.4, 1.1)
 animals[4,] = c("Moose", "Moose", "Any","Mammal", 140, 0.1, 0.3)
 animals[5,] = c("Goose - Canada", "Goose", "Any","Bird", 1.05, 2.3, 1.2)
 animals[6,] = c("Inconnu", "Inc/White", "Any","Fish", 2.55, 2.6, 1.5)
 animals[7,] = c("Char - Arctic", "Char - Arctic","Ulu", "Fish", 0.65, 2.4, 1.2)
 animals[8,] = c("Char - Arctic", "Char - Arctic","Paul", "Fish", 0.65, 2.4, 1.2)
 animals[9,] = c("Char - Arctic", "Char - Arctic","Akl", "Fish", 1.6, 2.4, 1.2)
 animals[10,] = c("Char - Arctic", "Char - Arctic","Sachs", "Fish", 0.7, 2.4, 1.2)
 animals[11,] = c("Char - Arctic", "Char - Arctic", "Tuk","Fish", 1, 2.4, 1.2)
 animals[12,] = c("Char - Arctic", "Char - Arctic","Inuvik", "Fish", 1.65, 2.4, 1.2)

 animals$edible_weight = as.numeric(animals$edible_weight)
 animals$log_harvest_per_encounter_mu = as.numeric(animals$log_harvest_per_encounter_mu)
 animals$log_harvest_per_encounter_sd = as.numeric(animals$log_harvest_per_encounter_sd)

 return(animals)
}

###########################################################################################
### function for simulation of a single hunting trip ###
###########################################################################################
simulate_a_trip = function(animals, parameters){ 
  # get species list
  species = unique(animals$species)

  # assign the trip to a village
  village = sample(parameters$villages, 1)
  
  # generate successful or unsuccessful harvests 
  p = inv_logit(parameters$fuel_success[1] + (parameters$fuel_success[2] * parameters$fuel))
  success = rbinom(1, size = 1, prob = 1-p)
  # (could improve by modeling as process until event)

  # choose a species
  if(!village %in% c("Tuk", "Inuvik")){
      harvest_type = sample(species, 1)
      } else {
      harvest_type = sample(species[which(species != "Char - Arctic")], 1)
  }
  
  # save whether trip was successful or not
  harvest_type_masked = ifelse(success==1, harvest_type, "No Harvest")

  # get the info about the animal harvested
  animals_subset = animals[which(animals$species==harvest_type), ]
  if(harvest_type == "Char - Arctic"){
   animals_subset = animals_subset[which(animals_subset$village==village),]
  }
  
  true_edible_weight = 9999 #catch to remove unbelievably large harvests
  while (true_edible_weight > 1050) { #max observed harvest is 1020 kg
    # simulate encounters 
    logmean_encounter = parameters$fuel_encounter[1] + parameters$fuel_encounter[2]*log(parameters$fuel)
    log_encounter = rnorm(1, mean=logmean_encounter, sd=parameters$fuel_encounter[3])
    #if(log_encounter <= 0){log_encounter==0} 
    encounter = exp(log_encounter)*success
  
    # simulate quantity of items conditional on encounter number
    #harvest_per_encounter = rlnorm(1, meanlog=animals_subset$log_harvest_per_encounter_mu, 
    #                                  sdlog=animals_subset$log_harvest_per_encounter_sd)
    harvest_per_encounter = exp(rnorm(1, mean=animals_subset$log_harvest_per_encounter_mu, 
                                   sd=animals_subset$log_harvest_per_encounter_sd))
    total_harvest = success*harvest_per_encounter
  
    # simulate reports of encounters, i.e., heaping
    #reporting_error_harvest = rlnorm(1, meanlog=log(total_harvest), sdlog=0.1)
    reporting_error_harvest = exp(rnorm(1, mean=log(total_harvest), sd=0.1))
    reported_harvest = round(reporting_error_harvest,-n_int_digits(reporting_error_harvest)) 
    reported_harvest = success*reported_harvest #re-zero the failures
  
    # simulate edible weight linked to reported encounters 
    reported_edible_weight = reported_harvest*animals_subset$edible_weight
    true_edible_weight = total_harvest*animals_subset$edible_weight

  }
  
  # organize results 
  results = data.frame(fuel_used = parameters$fuel, 
                       village = village, 
                       trip_successful = success, 
                       species = harvest_type_masked, 
                       encounter = encounter, 
                       total_harvest = total_harvest, 
                       reported_harvest = reported_harvest, 
                       reported_edible_weight = reported_edible_weight, 
                       true_edible_weight=true_edible_weight)

  return(results)
}

###########################################################################################
## Function for simulation of a season of hunting ##
###########################################################################################
simulate_a_harvest_season = function(N_trips, animals, parameters){ 
  results = vector("list", N_trips)

  for(i in 1:N_trips){
    # generate fuel use from beta distribution B(uv, (1-u)*v)
    alpha = parameters$fuel[2]*parameters$fuel[3]
    beta = (1-parameters$fuel[2])*parameters$fuel[3]
    fuel_used = rbeta(1, alpha, beta)*parameters$fuel[1]

   harvest_parameters = list(
    fuel = fuel_used,
    fuel_success = parameters$fuel_success,
    fuel_encounter = parameters$fuel_encounter,
    villages = parameters$villages
    )

  results[[i]] = simulate_a_trip(animals=animals, parameters=harvest_parameters)
  }

  results_df = do.call(rbind, results)

  if(global_parameters$mode=="All"){
    return(results_df)
  }

}

###########################################################################################
## Iterate model over simulations ##
###########################################################################################
iToraTor = function(animals, parameters){
 IHS = simulate_a_harvest_season(N_trips=2000, animals=animals, parameters=parameters)
 Toonik = simulate_a_harvest_season(N_trips=120, animals=animals, parameters=parameters)

 IHS_total_harvest = sum(IHS$true_edible_weight)
 IHS_sim_successes = IHS[which(IHS$trip_successful==1),]
 IHS_sim_failures = IHS[which(IHS$trip_successful==0),]

 dat = to_long_format(IHS, Toonik)

 model = cmdstanr::cmdstan_model("Code/Carbon_model.stan")
 fit = model$sample(data = dat,
                    seed = 4492,
                    chains = 1,
                    parallel_chains = 1,
                    refresh = 100,
                    iter_warmup = 500,
                    iter_sampling = 500
                    )

  stanfit = posterior::as_draws_rvars(fit$draws())

  fuel_usage_in = posterior::draws_of(stanfit$"fuel_usage_in")
  fuel_usage_out = posterior::draws_of(stanfit$"fuel_usage_out") 
  fuel_usage_all = posterior::draws_of(stanfit$"fuel_usage_all")
  harvest_total = posterior::draws_of(stanfit$"total_harvest_weight")

  res = matrix(NA, nrow=4, ncol=4)
  colnames(res) = c("In","Out", "All", "Harvest")
  rownames(res) = c("True", "Post_M", "Post_L", "Post_H")

  res[1,] = c(sum(IHS_sim_successes$fuel_used),
              sum(IHS_sim_failures$fuel_used),
              sum(IHS$fuel_used),
              IHS_total_harvest
              )

  res[2,] = c(median(fuel_usage_in),
              median(fuel_usage_out),
              median(fuel_usage_all),
              median(harvest_total)
              )

  res[3,] = c(HPDI(fuel_usage_in)[1],
              HPDI(fuel_usage_out)[1],
              HPDI(fuel_usage_all)[1],
              HPDI(harvest_total)[1]
              )

  res[4,] = c(HPDI(fuel_usage_in)[2],
              HPDI(fuel_usage_out)[2],
              HPDI(fuel_usage_all)[2],
              HPDI(harvest_total)[2]
              )

   return(res)
  }

  