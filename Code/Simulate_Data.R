# Code to produce simulated data for testing the model

# make a set of animals
foodsp_sim <- c("Caribou - Barren Ground", 
                "Seal - Ringed",
                "Hare - Arctic", 
                "Moose", 
                "Goose - Canada",
                "Inconnu",
                "Char - Arctic")
# some animals are grouped by ecological type
ecotype_sim <- c("Caribou",
                 "Seal - Ringed",
                 "Hare",
                 "Moose",
                 "Goose",
                 "Inc/White",
                 "Char")
# each animal will be assigned to a food class
markettype_sim <- c(rep("Mammal", 4),
                    rep("Bird", 1),
                    rep("Fish", 2))
# each animal will be assigned to a food class
markettype_sim_num <- c(rep(3, 4),
                    rep(1, 1),
                    rep(2, 2))
# harvest size characteristics for each animal type (log mean and sd)
logmeanharv_sim <- c(0.7, 0.9, 1.4, 0.1, 2.3, 2.6, 2.4)
sdmeanharv_sim <- c(0.7, 0.9, 1.1, 0.3, 1.2, 1.5, 1.2)
# edible weight of the animal
EW_sim <- c(36.5, 13, 2.9, 140, 1.05, 2.55, 9999) #fake N for indexing
#char are special: different in each community
EW_sim_char <-  c(0.65, 0.65, 1.6, 0.7, 1, 1.65)
#Ulu 6, Paul 3, Akl 1, Sachs 4 (technically no char in Tuk 5 or Inuvik 2)

# data frame representing our animal types
animals <- cbind.data.frame(foodsp_sim, ecotype_sim, markettype_sim, 
                                    logmeanharv_sim, sdmeanharv_sim)

sim_dat <- function(ntrips, animals, mode) { #add parameter argument so we can explore
  
  # generate successful and unsuccessful harvest trips
  nfailed <- rbinom(1, ntrips, 0.25) # 0.25 prob of a failed trip
  nsim <- ntrips-nfailed
  nsp <- length(animals$foodsp_sim)
  
  # gas use failed trips
  fuel_failed <- exp(rnorm(nfailed, 3, 1))
  
  # create the output for successful trips
  true_harv_sim <- meas_harv_sim <- EW_harv_sim <- numeric(nsim)
  # choose a species
  x <- sample(1:nsp, nsim, replace=TRUE)
  
  # assign to a village
  vill_samp <- sample(1:6, nsim, replace=TRUE)
  
  # for each hunt simulate a true harvest given the sampled species
  # then generate the reported harvest and the "true" edible weight
  for (i in 1:nsim) {
    #simulate a true harvest
    true_harv_sim[i] <- rlnorm(1, meanlog=animals$logmeanharv_sim[x[i]], 
                               sdlog=animals$sdmeanharv_sim[x[i]])
    # simulate the observation with error using scale factor 0.15
    meas_harv_sim[i] <- rlnorm(1, meanlog=log(true_harv_sim[i]), sdlog=0.15) # ROUND MORE?!
    # calculate the "true" EW
    if (x[i] != nsp) {EW_harv_sim[i] <- true_harv_sim[i]*EW_sim[x[i]]}
    if (x[i] == nsp) { #if it's char choose a community to determine the EW
      vill_samp[i] <- sample(c(1,3,4,6), 1, replace=FALSE) #reassign community
      EW_harv_sim[i] <- true_harv_sim[i]*EW_sim_char[vill_samp[i]]
    }
  }
  
  # fuel use of successful trips based a linear relationship with edible weight
  fuel_successful <- exp(rnorm(nsim, mean=(2 + 0.4*(log(EW_harv_sim))), sd=0.8))
  
  if (mode=="IHS") {
    dat <- cbind.data.frame(species=foodsp_sim[x], ecotype=ecotype_sim[x], 
                            community=vill_samp, markettype=markettype_sim_num[x],
                            true_harv_sim, meas_harv_sim, 
                            EW_harv_sim, fuel_successful)
    #later add some missing data, etc
  }
  
  if (mode=="Toonik") {
    dat <- cbind.data.frame(harvest=c(rep(0, nfailed), EW_harv_sim),
                                fuel=c(fuel_failed, fuel_successful))
  }
  
  return(dat)
}
