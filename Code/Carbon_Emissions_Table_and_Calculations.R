### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Table 3 (carbon emissions)
Table3 = data.frame(type=groupnames)
for (i in 1:4) {
  HPDI_temp = apply(samps$carbon_cost_est[,7,,i], 2, HPDI, 0.90)
  Table3 = cbind.data.frame(Table3,
                           lo=round(HPDI_temp[1,]/1000, 0), 
                           mean=round(apply(samps$carbon_cost_est[,7,,i], 
                                            2, mean)/1000, 0),
                           hi=round(HPDI_temp[2,]/1000, 0))
}
write.table(Table3, "table3.txt", sep="\t", row.names=FALSE)

# total emissions 
Table3[4,c(2, 13)] #lo-high ranges of HDPIs (???)
Table3[4,c(3, 6, 9, 12)] #scenario means

# emissions per kg food
Table3[4,c(3, 6, 9, 12)]*1000/Table1[4,1]

# emissions per beneficiary
Table3[4,c(3, 6, 9, 12)]*1000/2767

# slope of fuel regression
mean(samps$b)
sd(samps$b)

# prob failed trip
mean(samps$theta)
sd(samps$theta)

# input gasoline (l)
input = apply(samps$fuel_obs_est, 2, mean)
input
apply(samps$fuel_obs_est, 2, sd)

# n failed trips
apply(samps$nzero, 2, mean)
apply(samps$nzero, 2, sd)


# additional gas (l)
apply(samps$fuel_zero_est, 2, mean)
apply(samps$fuel_zero_est, 2, sd)

#total gas (l)
totallitres = apply(samps$fuel_total, 2, mean)
totallitres
apply(samps$fuel_total, 2, sd)

# total input gasoline ($)
totalgascost = apply(samps$fuel_total_cost, 2, mean)
apply(samps$fuel_total_cost, 2, sd)

# total input gasoline (CO2)
lo_gas_emissions = apply(samps$fuel_total_emissions[,,1], 2, mean) #lo barge
high_gas_emissions = apply(samps$fuel_total_emissions[,,2], 2, mean) #high barge
apply(samps$fuel_total_emissions[,,1], 2, HPDI) #lo barge
apply(samps$fuel_total_emissions[,,2], 2, HPDI)

lo_gas_emissions/1000 
high_gas_emissions/1000

# litres, $ and Co2 per kilo harvested
cbind.data.frame(Settlement=c(villages, "Total"), 
                   litres = totallitres/comkgtotal,
                   cost=totalgascost/comkgtotal,
                   lo=lo_gas_emissions/comkgtotal,
                   hi=high_gas_emissions/comkgtotal)
# I think in Tuk they are more efficient because they get these big catches of fish 
# Points to a weakness in our method

# kg carbon per person
lo_gas_emissions/c(bene2021, 2767)
high_gas_emissions/c(bene2021, 2767)

