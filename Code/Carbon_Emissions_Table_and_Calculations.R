### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Table 3 (carbon emissions)
Table3 = data.frame(type=c("Low Barge", "High Barge", "Low Food Mail", "High Food Mail"))

for (i in 1:4) {
  HPDI_temp = matrix(unlist(lapply(emissions[[i]], HPDI, 0.90)), ncol=2, byrow=TRUE)
  Table3 = cbind.data.frame(Table3, 
                            lo=HPDI_temp[,1]/1000,
                            mean=unlist(lapply(emissions[[i]], mean))/1000,
                            hi=HPDI_temp[,2]/1000)
                           
}
write.table(Table3, "table3.txt", sep="\t", row.names=FALSE)

# total emissions 
print("Total emissions by scenario")
print(Table3[,12]) #scenario means

# emissions per kg food
print("Emissions per kg")
print(Table3[,12]*1000/Table1[4,1])

# emissions per beneficiary
print("Emissions per beneficiary")
print(Table3[,12]*1000/2767)

# prob failed trip
print("Prob failed trip (theta)")
print(inv_logit(mean(samps$theta)))
#print(sd(samps$theta))

# input gasoline successes (l)
print("Gasoline used in successful trips, mean and sd")
print(mean(samps$fuel_usage_in))
print(sd(samps$fuel_usage_in))

# n failed trips
print("Number failed trips, mean and sd")
print(mean(apply(samps$N_failures, 1, sum)))
print(sd(apply(samps$N_failures, 1, sum)))

# additional gas for failures (l)
print("Gasoline used in failed trips, mean and sd")
print(mean(samps$fuel_usage_out))
print(sd(samps$fuel_usage_out))

#total gas (l)
print("Total gasoline used, mean and sd")
print(mean(samps$fuel_usage_all))
print(sd(samps$fuel_usage_all))

# total input gasoline ($)
totalgascost = apply(samps$fuel_costs, 1, sum) + apply(samps$fuel_failures_costs, 1, sum)
print("Total gasoline costs, mean and sd")
print(mean(totalgascost))
print(sd(totalgascost))

# total input gasoline (CO2)
lo_gas_emissions = apply(samps$fuel_emissions_low, 1, sum) + 
  apply(samps$fuel_failures_low_emissions, 1, sum)
high_gas_emissions = apply(samps$fuel_emissions_high, 1, sum) + 
  apply(samps$fuel_failures_high_emissions, 1, sum)

print("Emissions from gasoline, lo and high")
print(mean(lo_gas_emissions)/1000)
print(mean(high_gas_emissions)/1000)

print("Emissions from gasoline, HPDI low scenario")
print(HPDI(lo_gas_emissions, 0.90)/1000)

print("Emissions from gasoline, HPDI high scenario")
print(HPDI(high_gas_emissions, 0.90)/1000)

# litres, $ and Co2 per kilo harvested
print("Litres, $ and Co2 fuel consumption per kilo harvested")
print(cbind.data.frame(litres = mean(samps$fuel_usage_all)/Table1[4,1],
                 cost=mean(totalgascost)/Table1[4,1],
                 lo=mean(lo_gas_emissions)/Table1[4,1],
                 hi=mean(high_gas_emissions)/Table1[4,1]))

# kg carbon per person
print("Kg carbon per person in fuel used")
print(mean(lo_gas_emissions)/2767)
print(mean(high_gas_emissions)/2767)

