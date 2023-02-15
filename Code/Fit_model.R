### THE MODEL ###
N = length(harv_edible$NumHarvest)
x = 0.15 # scaling factor
dat3 = list(EW = d$ADJ_HARV[idx],
             Fuel = d$gaslitres[idx],
             hunter = as.numeric(as.factor(d$HUNTER[idx])), 
             community = as.numeric(as.factor(harv_edible$Community)),
             harv_meas = harv_edible$NumHarv,
             EW_miss = harv_edible$missing,
             species = as.numeric(as.factor(harv_edible$SpeciesNam)),
             ecotype = as.numeric(as.factor(harv_edible$ecotype)),
             markettype = as.numeric(as.factor(harv_edible$markettype)),
             N = length(harv_edible$NumHarvest),
             log_meanharv_eco = log_meanharv_eco,
             log_sdharv_eco = log_sdharv_eco,
             Nsp = length(unique(as.numeric(as.factor(harv_edible$SpeciesNam)))),
             tripsperc = as.numeric(table(harv_edible$Community)),
             Necotype = length(unique(as.numeric(as.factor(harv_edible$ecotype)))),
             spEW=c(EWorder, 0.65, 0.65, 1.6, 0.7, 1, 1.65), 
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

#small version for testing 
#small_samps = extract.samples(harvm3, n=100)
#write.csv(small_samps, "posterior_samples_100.csv")

#read in samps if not loaded
#samps <- read.csv(...)

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

