
###########################################################################################
### function for simulation of a single hunting trip ###
###########################################################################################
simulate_a_trip <- function(animals, parameters) {
    # get species list
    species = unique(animals$species)
    
    # assign the trip to a village
    village = sample(parameters$villages, 1)
    
    # generate successful or unsuccessful harvests 
    success = rbinom(1, size = 1, prob = 1-parameters$theta)
    
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
    
    #generate fuel use for unsuccessful trips

    
    true_edible_weight = 9999 #catch to remove unbelievably large harvests
    while (true_edible_weight > 1050) { #max observed harvest is 1020 kg

      # simulate quantity of items conditional on encounter number
      harvest_per_trip = exp(rnorm(1, mean=animals_subset$log_harvest_per_encounter_mu, 
                                   sd=animals_subset$log_harvest_per_encounter_sd))
      total_harvest = success*harvest_per_trip
      
      # simulate reports of encounters, i.e., heaping
      reporting_error_harvest = exp(rnorm(1, mean=log(total_harvest), sd=0.1))
      reported_harvest = round(reporting_error_harvest,-n_int_digits(reporting_error_harvest)) 
      reported_harvest = success*reported_harvest #re-zero the failures
      
      # simulate edible weight linked to reported encounters 
      reported_edible_weight = reported_harvest*animals_subset$edible_weight
      true_edible_weight = total_harvest*animals_subset$edible_weight
      
      if (success == 1) {
        fuel<- exp(rnorm(1, mean=(parameters$fuel_success_alpha + 
                         parameters$fuel_success_beta*(log(total_harvest))), 
                         sd=parameters$fuel_success_sd))
      }
      
    }
    
    if (success == 0) {
      fuel <- exp(rnorm(1, parameters$fuel_fail_alpha, parameters$fuel_fail_sd))
    }
    
    # organize results 
    results = data.frame(fuel_used = fuel, 
                         village = village, 
                         trip_successful = success, 
                         species = harvest_type_masked, 
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
    harvest_parameters = list(
      theta = parameters$theta,
      fuel_fail_alpha = parameters$fuel_fail_alpha, 
      fuel_fail_sd = parameters$fuel_fail_sd,
      fuel_success_alpha = parameters$fuel_success_alpha,
      fuel_success_beta = parameters$fuel_success_beta,
      fuel_success_sd = parameters$fuel_success_sd,
      villages = parameters$villages
    )
    
    results[[i]] = simulate_a_trip(animals=animals, parameters=harvest_parameters)
  }
  
  results_df = do.call(rbind, results)
  
  if(global_parameters$mode=="All"){
    return(results_df)
  }
}