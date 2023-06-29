####################################### Load libraries
library(foreign)
library(rlang)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)

####################################### Set working directory
setwd("/Users/elspethready/repos/inuvialuit_carbon")

####################################### Load some needed info
villages = c("Aklavik", "Inuvik", "Paulatuk", "SachsHarbour", 
             "Tuktoyaktuk", "Ulukhaktok")

####################################### Estimation of emissions (per kg)
source("Code/Calculate_Emissions.R")

####################################### Estimation of emissions (per kg)
source("Code/Simulate_Data.R")

IHS_sim <- sim_dat(4000, animals, mode="IHS")
Toonik_sim <- sim_dat(120, animals, mode="Toonik")

N = length(IHS_sim$species)
x = 0.15 # scaling factor
dat3 = list(EW = Toonik_sim$harvest,
            Fuel = Toonik_sim$fuel,
            community = IHS_sim$community,
            harv_meas = IHS_sim$meas_harv_sim,
            EW_miss = rep(0, N), #NOT YET IMPLEMENTED IN SIM
            species = as.numeric(factor(IHS_sim$species, levels=foodsp_sim)),
            ecotype = as.numeric(factor(IHS_sim$ecotype, levels=ecotype_sim)),
            markettype = IHS_sim$markettype,
            N = N,
            N_took = length(Toonik_sim$harvest),
            log_meanharv_eco = logmeanharv_sim,
            log_sdharv_eco = sdmeanharv_sim,
            Nsp = length(unique(IHS_sim$species)),
            tripsperc = as.numeric(table(IHS_sim$community)),
            Necotype = length(unique(IHS_sim$ecotype)),
            spEW=c(EW_sim, EW_sim_char), 
            scale_factor_error=x,
            market_costs = market_costs,
            market_emissions = market_emissions, #array of carbon scenarios
            fuel_cost = 1.76,
            fuel_emissions = rail_emissions_litre[,2:3],
            sim=1)

harvm3 <- stan(file = "Code/Carbon_model.stan", data = dat3, 
               control=list(adapt_delta=0.99, max_treedepth=20), 
               iter=1000, chains=1, seed=4492)

# Save samps for working without rerunning model
samps = extract.samples(harvm3)
str(samps)

plot(samps$EW_est[,1], type="l") # Plot a few to check mixing
plot(samps$EW_est[,1697], type="l") 
plot(samps$market_cost_est[,7,2], type="l") 
plot(samps$fuel_total_cost[,7], type="l") 

# parameters to compare

# Fuel model
samps$a #fuel intercept (success; 2)
samps$b #fuel slope (0.4)
samps$a2 # fuel intercept (failure; 3)
samps$phi[1] #fuel error (success; 0.8)
samps$phi[2] # error 
samps$theta

par(mfrow=c(6,1), mar=c(3,0,3,0))
plot(density(samps$theta), axes=TRUE, ylab="", main="Theta (Probability failure)")
abline(v=0.25, col="red")
plot(density(samps$a), axes=TRUE, ylab="", main="a (Fuel intercept)")
abline(v=2, col="red")
plot(density(samps$b), axes=TRUE,  ylab="", main="b (Fuel slope)")
abline(v=0.4, col="red")
plot(density(samps$phi[,1]), axes=TRUE, ylab="", main="phi[1] (Fuel error linear predictor)")
abline(v=0.8, col="red")
plot(density(samps$a2), axes=TRUE, ylab="", main="a2 (Fuel intercept; failure)")
abline(v=3, col="red")
plot(density(samps$phi[,2]), axes=TRUE, ylab="", main="phi[2] (Fuel error; failure)")
abline(v=1, col="red")

# Measurement model
mean_harv_heap <- apply(samps$harv_true, 2, median)
mean_harv_sim <- apply(samps$sim_harv, 2, median)
mean_fuel_est <- apply(samps$sim_harv, 2, median)

par(mfrow=c(3,1), mar=c(2,0,3,0))
IHS_sim$harv_error <- mean_harv_sim-IHS_sim$true_harv_sim
hist(IHS_sim$harv_error, breaks=1000, main="Estimate minus true harvest size")
IHS_sim$EW_error <- apply(samps$EW_est, 2, mean) - IHS_sim$EW_harv_sim
hist(IHS_sim$EW_error, breaks=1000, main="Estimate minus true harvest (edible weight)")
IHS_sim$fuel_error <- mean_fuel_est - IHS_sim$fuel_successful
hist(IHS_sim$fuel_error, breaks=1000, main="Estimate minus true fuel use")

#what are the outliers
#OK so we have some problems with big fish harvests (makes sense)
IHS_sim[which(abs(IHS_sim$harv_error)>100),]
IHS_sim[which(abs(IHS_sim$EW_error)>100),]
IHS_sim[which(abs(IHS_sim$fuel_error)>100),]


