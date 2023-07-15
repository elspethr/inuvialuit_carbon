### THE MODEL ###

## data
N = length(harv_edible$NumHarvest)
N_took = length(d$ADJ_HARV[idx])
x = 0.15 # scaling factor

dat3 = list(# Toonik data
            N_took = N_took,
            EW = d$ADJ_HARV[idx],
            Fuel = d$gaslitres[idx],
            #hunter = as.numeric(as.factor(d$HUNTER[idx])), 
            # IHS data
            community = vill_id,
            harv_meas = harv_edible$NumHarv,
            EW_miss = harv_edible$missing,
            species = species_id,
            ecotype = ecotype_id,
            markettype = markettype_id,
            log_meanharv_eco = harv_edible$log_meanharv_eco,
            log_sdharv_eco= harv_edible$log_sdharv_eco,
            N = N,
            Nsp = length(unique(as.numeric(as.factor(harv_edible$SpeciesNam)))),
            tripsperc = as.numeric(table(harv_edible$Community)),
            Necotype = length(unique(as.numeric(as.factor(harv_edible$ecotype)))),
            spEW = harv_edible$spEW,
            scale_factor_error=x,
            # cost and carbon scenarios
            market_scalar = harv_edible$mc,
            scalar_barge_low = harv_edible$barge_low,
            scalar_barge_high = harv_edible$barge_high,
            scalar_mail_low = harv_edible$mail_low,
            scalar_mail_high = harv_edible$mail_high,
            fuel_cost = 1.76,
            scalar_fuel_emissions_low = harv_edible$fuel_emissions_lo,
            scalar_fuel_emissions_high = harv_edible$fuel_emissions_high,
            scalar_fuel_emissions_villages = rail_emissions_litre[,2:3],
            sim=0)

## run it!
harvm3 <- stan(file = "Code/Carbon_model.stan", data = dat3, 
               control=list(adapt_delta=0.99, max_treedepth=20), 
               iter=4000, chains=3, seed=4492)

# save samps for working without rerunning model
samps = extract.samples(harvm3)
str(samps)

#small version for testing 
#small_samps = extract.samples(harvm3, n=100)
#write.csv(small_samps, "posterior_samples_100.csv")

#read in samps if not loaded
#samps <- read.csv(...)

## check mixing
plot(samps$theta, type="l") 
plot(samps$log_harv_true[,1697], type="l") 
plot(samps$alpha[,2], type="l") 

## output harvest estimates for sanity check
sim_90lo = sim_90hi = numeric(N)
for (i in 1:N) {
  HPDI_perh = HPDI(samps$harvest_number[,i], 0.90)
  sim_90lo[i] = HPDI_perh[1]
  sim_90hi[i] = HPDI_perh[2]
}
write.csv(cbind.data.frame(species=harv_edible$SpeciesNam, report=dat3$harv_meas,
                           mean=apply(samps$harvest_number, 2, mean), 
                           loHPDI=sim_90lo, hiHDPI=sim_90hi), 
          "simulated_harvs.csv")

