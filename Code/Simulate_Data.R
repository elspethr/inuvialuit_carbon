### Code to produce simulated data for testing the model ###

## First create a database of animals

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

## Functions for simulation

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
  
  # simulate encounters 
  logmean_encounter = parameters$fuel_encounter[1] + parameters$fuel_encounter[2]*log(parameters$fuel)
  log_encounter = rnorm(1, mean=logmean_encounter, sd=parameters$fuel_encounter[3])
  encounter = exp(log_encounter)*success
  #encounter = encountered

  # simulate quantity of items conditional on encounter number
  harvest_per_encounter = rlnorm(1, meanlog=animals_subset$log_harvest_per_encounter_mu, 
                                 sdlog=animals_subset$log_harvest_per_encounter_sd)
  total_harvest = encounter*harvest_per_encounter
  

  # simulate reports of encounters, i.e., heaping
  #DEAL WITH os
  reporting_error_harvest = rlnorm(1, meanlog=log(total_harvest), sdlog=0.15)
  reported_harvest = round(reporting_error_harvest, -n_int_digits(reporting_error_harvest)) 
  # if report < 1 round to one instead?
  reported_harvest = success*reported_harvest #re-zero the failures

  # simulate edible weight linked to reported encounters 
  reported_edible_weight = reported_harvest*animals_subset$edible_weight
  true_edible_weight = total_harvest*animals_subset$edible_weight
  
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

simulate_a_harvest_season = function(N_trips, animals, global_parameters){ 
  results = vector("list", N_trips)

  for(i in 1:N_trips){
    # generate fuel use from beta distribution B(uv, (1-u)*v)
    alpha = global_parameters$fuel[2]*global_parameters$fuel[3]
    beta = (1-global_parameters$fuel[2])*global_parameters$fuel[3]
    fuel_used = rbeta(1, alpha, beta)*global_parameters$fuel[1]

   harvest_parameters = list(
    fuel = fuel_used,
    fuel_success = global_parameters$fuel_success,
    fuel_encounter = global_parameters$fuel_encounter,
    villages = global_parameters$villages
    )

  results[[i]] = simulate_a_trip(animals, harvest_parameters)
  }

  results_df = do.call(rbind, results)

  #if(global_parameters$mode=="IHS"){
  #  return(data.frame(harvest_true=results_df$total_harvest,
  #                    harvest_reported=results_df$reported_harvest,
  #                    EW=results_df$reported_edible_weight,
  #                    true_EW=results_df$true_edible_weight,
  #                   village = result_dfs$village,
  #                   species = results_df$species,
  #                    fuel=results_df$fuel_used))
  #}
  
  #if(global_parameters$mode=="Toonik"){
  #  return(data.frame(EW=results_df$reported_edible_weight, 
  #                    fuel=results_df$fuel_used))
  #}

  if(global_parameters$mode=="All"){
    return(results_df)
  }

}

## Now run simulation 

 global_parameters = list(
    fuel = c(100, 0.17, 5), #scalar, u, v
    fuel_encounters = c(-1.95, 0.9, 0.25), #intercept and slope for mean, sd
    fuel_success = c(-3.5, 0.125), #intercept, slope of logistic reg
    villages = c( "Akl", "Inuvik", "Paul", "Sachs", "Tuk", "Ulu"),
    mode = "All"
  )

#bob = simulate_a_harvest_season(1000, animals, global_parameters)
#round_df(head(bob, 25))





