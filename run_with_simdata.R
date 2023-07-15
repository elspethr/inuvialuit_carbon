### Load libraries ###
library(foreign)
library(rlang)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)

### Set working directory ###
#setwd("/Users/elspethready/repos/inuvialuit_carbon")

### Load some needed info ###
villages = c("Aklavik", "Inuvik", "Paulatuk", "SachsHarbour", 
             "Tuktoyaktuk", "Ulukhaktok")


### Source needed functions ###
source("Code/Functions.R")

### Estimation of emissions (per kg) ###
source("Code/Calculate_Emissions.R")

### Simulate data ###
source("Code/Simulate_Data.R")

IHS_sim <- simulate_a_harvest_season(2000, animals, global_parameters)
IHS_sim_successes <- IHS_sim[which(IHS_sim$trip_successful==1),]
IHS_sim_failures <- IHS_sim[which(IHS_sim$trip_successful==0),]

Toonik_sim <- simulate_a_harvest_season(120, animals, global_parameters)

# (organize all model input data into long format)
IHS_sim_successes$species_id <- as.numeric(factor(IHS_sim_successes$species, 
                                                  levels=unique(animals$species)))
IHS_sim_successes$ecotype_id  <- as.numeric(factor(animals$ecotype[IHS_sim_successes$species_id], 
                                            levels=unique(animals$ecotype)))

IHS_sim_successes$log_meanharv_eco = animals$log_harvest_per_encounter_mu[IHS_sim_successes$ecotype_id]
IHS_sim_successes$log_sdharv_eco = animals$log_harvest_per_encounter_sd[IHS_sim_successes$ecotype_id]

vill_id = as.numeric(factor(IHS_sim_successes$village))
markettype_id = as.numeric(factor(animals$markettype[IHS_sim_successes$species_id]))

for (i in 1:length(IHS_sim_successes$species)){
  if (IHS_sim_successes$species[i] %in% c("Char - Arctic", "Char - Dolly Varden")) {
    IHS_sim_successes$spEW[i] <- c(0.65, 0.65, 1.6, 0.7, 1, 1.65)[vill_id[i]]
  }
  else {
    IHS_sim_successes$spEW[i] <- animals$edible_weight[IHS_sim_successes$species_id[i]]
  }
  IHS_sim_successes$mc[i] <- market_costs[vill_id[i], markettype_id[i]]
  IHS_sim_successes$barge_low[i] <- market_emissions[vill_id[i], 1, markettype_id[i]]
  IHS_sim_successes$barge_high[i] <- market_emissions[vill_id[i], 2, markettype_id[i]]
  IHS_sim_successes$mail_low[i] <- market_emissions[vill_id[i], 3, markettype_id[i]]
  IHS_sim_successes$mail_high[i] <- market_emissions[vill_id[i], 4, markettype_id[i]]
  IHS_sim_successes$fuel_emissions_lo[i] <- rail_emissions_litre[vill_id[i],2]
  IHS_sim_successes$fuel_emissions_high[i] <- rail_emissions_litre[vill_id[i],3]
}

scalar_fuel_emissions_villages = rail_emissions_litre[,2:3]

N = length(IHS_sim_successes$species)
x = 0.15 # scaling factor
dat3 = list(EW = Toonik_sim$reported_edible_weight, #or use true???
            Fuel = Toonik_sim$fuel_used,
            community = vill_id,
            harv_meas = IHS_sim_successes$reported_harvest,
            EW_miss = rep(0, N), #MISSING DATA NOT YET IMPLEMENTED IN SIM
            species = IHS_sim_successes$species_id,
            ecotype = IHS_sim_successes$ecotype_id,
            markettype = markettype_id,
            N = N,
            N_took = length(Toonik_sim$reported_edible_weight),
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

harvm3 <- stan(file = "Code/Carbon_model.stan", data = dat3, 
               control=list(adapt_delta=0.99, max_treedepth=20), 
               iter=1000, chains=1, seed=4492)

# Save samps for working without rerunning model
samps = extract.samples(harvm3)
str(samps)

plot(samps$log_harv_true[,1], type="l") # Plot a few to check mixing
plot(samps$theta, type="l") 
plot(samps$beta, type="l") 
plot(samps$alpha[,1], type="l") 

# parameters to compare

# Fuel model
#samps$alpha[,1] #fuel intercept (success; 2)
#samps$alpha[,2] #fuel slope (0.4)
#samps$phi_alpha #fuel error (success; 0.8)
#samps$beta # fuel intercept (failure; 3)
#samps$phi_beta # failure fuel error 
#samps$theta # failure theta

par(mfrow=c(6,1), mar=c(3,0,3,0))
plot(density(exp(samps$theta)), axes=TRUE, ylab="", main="theta (Probability failure)")
abline(v=0.25, col="red")
plot(density(samps$alpha[,1]), axes=TRUE, ylab="", main="alpha1 (Fuel intercept)")
abline(v=2, col="red")
plot(density(samps$alpha[,2]), axes=TRUE,  ylab="", main="alpha2 (Fuel slope)")
abline(v=0.4, col="red")
plot(density(samps$phi_alpha), axes=TRUE, ylab="", main="phi_alpha (Fuel error linear predictor)")
abline(v=0.8, col="red")
plot(density(samps$beta), axes=TRUE, ylab="", main="beta (Fuel intercept; failure)")
abline(v=3, col="red")
plot(density(samps$phi_beta[,2]), axes=TRUE, ylab="", main="phi_beta (Fuel error; failure)")
abline(v=1, col="red")


## OLD tests, not working ##
# Measurement model
#mean_harv_heap <- apply(samps$harv_true, 2, median)
#mean_harv_sim <- apply(samps$sim_harv, 2, median)
#mean_fuel_est <- apply(samps$sim_harv, 2, median)

#par(mfrow=c(3,1), mar=c(2,0,3,0))
#IHS_sim$harv_error <- mean_harv_sim-IHS_sim$true_harv_sim
#hist(IHS_sim$harv_error, breaks=1000, main="Estimate minus true harvest size")
#IHS_sim$EW_error <- apply(samps$EW_est, 2, mean) - IHS_sim$EW_harv_sim
#hist(IHS_sim$EW_error, breaks=1000, main="Estimate minus true harvest (edible weight)")
#IHS_sim$fuel_error <- mean_fuel_est - IHS_sim$fuel_successful
#hist(IHS_sim$fuel_error, breaks=1000, main="Estimate minus true fuel use")

#what are the outliers
#OK so we have some problems with big fish harvests (makes sense)
#IHS_sim[which(abs(IHS_sim$harv_error)>100),]
#IHS_sim[which(abs(IHS_sim$EW_error)>100),]
#IHS_sim[which(abs(IHS_sim$fuel_error)>100),]


