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
# harvest size characteristics for each animal type (log mean and sd)
logmeanharv_sim <- c(0.7, 0.9, 1.4, 0.1, 2.3, 2.6, 2.4)
sdmeanharv_sim <- c(0.7, 0.9, 1.1, 0.3, 1.2,1.5, 1.2)
# edible weight of the animal
EW_sim <- c(36.5, 13, 2.9, 140, 1.05, 2.55)
#char are special: different in each community
EW_sim_char <- c(1.55, 1.6, 0.65, 0.7) 
#Ulu, Paul, Akl, Sachs (no char in Tuk or Inuvik)

# data frame representing our animal types
animals_for_sim <- cbind.data.frame(foodsp_sim, ecotype_sim, markettype_sim, 
                                    logmeanharv_sim, sdmeanharv_sim)
nsp <- length(foodsp_sim)

# generate successful and unsuccessful harvest trips
ntrips <- 5000
nfailed <- rbinom(1, 5000, 0.25) #25% prob of a failed trip
nsim <- ntrips-nfailed

# gas use failed trips
fuel_failed <- exp(rnorm(nfailed, 3, 1))

# create of the output for successful trips
true_harv_sim <- meas_harv_sim <- numeric(nsim)
# choose a species
x <- sample(1:nsp, nsim, replace=TRUE)
simIHSdat_sp <- foodsp_sim[x]
simIHSdat_ecotype <- ecotype_sim[x]
# for each hunt simulate a true harvest given the sampled species
# then generate the reported harvest and the "true" edible weight
for (i in 1:nsim) {
  #simulate a true harvest
  true_harv_sim[i] <- rlnorm(1, meanlog=logmeanharv_sim[x[i]], sdlog=sdmeanharv_sim[x[i]])
  # simulate the observation with error using scale factor 0.15
  meas_harv_sim[i] <- rlnorm(1, meanlog=log(true_harv_sim[i]), sdlog=0.15)
  # calculate the "true" EW
  if (x[i] != 7) {EW_harv_sim[i] <- true_harv_sim[i]*EW_sim[x[i]]}
  if (x[i] == 7) { #if it's char choose a community to determine the EW
    vill_samp <- sample(1:4, 1, replace=FALSE)
    EW_harv_sim[i] <- true_harv_sim[i]*EW_sim_char[vill_samp]
  }
}
# fuel use based a linear relationship with edible weight
fuel_successful <- exp(rnorm(nsim, mean=(2 + 0.4*(log(EW_harv_sim))), sd=0.8))

# make a nice data frame for our pretend harvests
sim_dat <- cbind.data.frame(simIHSdat_sp, true_harv_sim, meas_harv_sim, EW_harv_sim, fuel_successful)
