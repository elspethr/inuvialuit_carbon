####################################### Load libraries
library(foreign)
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
            fuel_emissions = rail_emissions_litre[,2:3])

harvm3 <- stan(file = "Code/Carbon_model.stan", data = dat3, 
               control=list(adapt_delta=0.99, max_treedepth=20), 
               iter=4000, chains=3, seed=4492)

# Save samps for working without rerunning model
samps = extract.samples(harvm3)
str(samps)

plot(samps$EW_est[,1], type="l") # Plot a few to check mixing
plot(samps$EW_est[,1697], type="l") 
plot(samps$market_cost_est[,7,2], type="l") 
plot(samps$fuel_total_cost[,7], type="l") 

# Output harvest estimates for sanity check
sim_90lo = sim_90hi = numeric(N)
for (i in 1:N) {
  HPDI_perh = HPDI(samps$sim_harv[,i], 0.90)
  sim_90lo[i] = HPDI_perh[1]
  sim_90hi[i] = HPDI_perh[2]
}
write.csv(cbind.data.frame(species=harv_edible$SpeciesNam, report=dat3$harv_meas,
                           mean=apply(samps$sim_harv, 2, mean), 
                           loHPDI=sim_90lo, hiHDPI=sim_90hi), 
          "simulated_harvs.csv")

