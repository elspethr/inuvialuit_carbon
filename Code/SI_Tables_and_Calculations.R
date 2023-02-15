### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Per capita harvest in diff villages
cbind(villages, comkgtotal[1:6]/bene2021)
(comkgtotal[1:6]/bene2021)[3]/0.6 #adjusted for sampling?

# additional subsidy for amount harvestes
# Aklavik, Paulatuk, Sachs Harbour, and Ulukhaktok in 2020 were $5.75, $4.95, $7.25, $5.65 
extrasubsidy <- comkgtotal[1:6]*c(5.75, 0, 4.95, 7.25, 0, 5.65)
comCADtotal+c(extrasubsidy, sum(extrasubsidy))


# Table S7
pars = c("Theta (prob. failed trip)", "Fuel intercept - successful", "Fuel slope - successful", "Standard deviation, linear estimator", "Fuel intercept - failed trip", "Fuel sd - failed trip")
mean = round(c(mean(samps$theta), mean(samps$a), mean(samps$b), 
          mean(samps$phi[,1]), mean(samps$a2), mean(samps$phi[,2])), 3)
sds = round(c(sd(samps$theta), sd(samps$a), sd(samps$b), 
          sd(samps$phi[,1]), sd(samps$a2), sd(samps$phi[,2])),3) 

TableS7 = cbind.data.frame(Parameter=pars, Mean=mean, SD=sds)
write.table(TableS7, "tableS7.txt", sep="\t", row.names=FALSE)

# Additional tables with village breakdowns

Birds_EW_lo <- apply(samps$harvest_est[,,1], 2, HPDI, 0.90)[1,]
Birds_EW_mean <- apply(samps$harvest_est[,,1], 2, mean)
Birds_EW_hi <- apply(samps$harvest_est[,,1], 2, HPDI, 0.90)[2,]
Fish_EW_lo <- apply(samps$harvest_est[,,2], 2, HPDI, 0.90)[1,]
Fish_EW_mean <- apply(samps$harvest_est[,,2], 2, mean)
Fish_EW_hi <- apply(samps$harvest_est[,,2], 2, HPDI, 0.90)[2,]
Mammals_EW_lo <- apply(samps$harvest_est[,,3], 2, HPDI, 0.90)[1,]
Mammals_EW_mean <- apply(samps$harvest_est[,,3], 2, mean)
Mammals_EW_hi <- apply(samps$harvest_est[,,3], 2, HPDI, 0.90)[2,]
Total_EW_lo <- apply(samps$harvest_est[,,4], 2, HPDI, 0.90)[1,]
Total_EW_mean <- apply(samps$harvest_est[,,4], 2, mean)
Total_EW_high <- apply(samps$harvest_est[,,4], 2, HPDI, 0.90)[2,]

write.table(cbind.data.frame(Birds_EW_lo, Birds_EW_mean, Birds_EW_hi, Fish_EW_lo, Fish_EW_mean, Fish_EW_hi, Mammals_EW_lo, Mammals_EW_mean, Mammals_EW_hi, Total_EW_lo, Total_EW_mean, Total_EW_high), "EW_by_village.txt", sep="\t", row.names=FALSE)

Birds_cost_lo <- apply(samps$market_cost_est[,,1], 2, HPDI, 0.90)[1,]
Birds_cost_mean <- apply(samps$market_cost_est[,,1], 2, mean)
Birds_cost_hi <- apply(samps$market_cost_est[,,1], 2, HPDI, 0.90)[2,]
Fish_cost_lo <- apply(samps$market_cost_est[,,2], 2, HPDI, 0.90)[1,]
Fish_cost_mean <- apply(samps$market_cost_est[,,2], 2, mean)
Fish_cost_hi <- apply(samps$market_cost_est[,,2], 2, HPDI, 0.90)[2,]
Mammals_cost_lo <- apply(samps$market_cost_est[,,3], 2, HPDI, 0.90)[1,]
Mammals_cost_mean <- apply(samps$market_cost_est[,,3], 2, mean)
Mammals_cost_hi <- apply(samps$market_cost_est[,,3], 2, HPDI, 0.90)[2,]
Total_cost_lo <- apply(samps$market_cost_est[,,4], 2, HPDI, 0.90)[1,]
Total_cost_mean <- apply(samps$market_cost_est[,,4], 2, mean)
Total_cost_high <- apply(samps$market_cost_est[,,4], 2, HPDI, 0.90)[2,]

write.table(cbind.data.frame(Birds_cost_lo, Birds_cost_mean, Birds_cost_hi, Fish_cost_lo, Fish_cost_mean, Fish_cost_hi, Mammals_cost_lo, Mammals_cost_mean, Mammals_cost_hi, Total_cost_lo, Total_cost_mean, Total_cost_high), "cost_by_village.txt", sep="\t", row.names=FALSE)

full_carbon_scenarios <- data.frame()
for (i in 1:4) {
  Birds_cost_lo <- apply(samps$carbon_cost_est[,,1,i], 2, HPDI, 0.90)[1,]/1000
  Birds_cost_mean <- apply(samps$carbon_cost_est[,,1,i], 2, mean)/1000
  Birds_cost_hi <- apply(samps$carbon_cost_est[,,1,i], 2, HPDI, 0.90)[2,]/1000
  Fish_cost_lo <- apply(samps$carbon_cost_est[,,2,i], 2, HPDI, 0.90)[1,]/1000
  Fish_cost_mean <- apply(samps$carbon_cost_est[,,2,i], 2, mean)/1000
  Fish_cost_hi <- apply(samps$carbon_cost_est[,,2,i], 2, HPDI, 0.90)[2,]/1000
  Mammals_cost_lo <- apply(samps$carbon_cost_est[,,3,i], 2, HPDI, 0.90)[1,]/1000
  Mammals_cost_mean <- apply(samps$carbon_cost_est[,,3,i], 2, mean)/1000
  Mammals_cost_hi <- apply(samps$carbon_cost_est[,,3,i], 2, HPDI, 0.90)[2,]/1000
  Total_cost_lo <- apply(samps$carbon_cost_est[,,4,i], 2, HPDI, 0.90)[1,]/1000
  Total_cost_mean <- apply(samps$carbon_cost_est[,,4,i], 2, mean)/1000
  Total_cost_high <- apply(samps$carbon_cost_est[,,4,i], 2, HPDI, 0.90)[2,]/1000
  temp <- cbind.data.frame(Birds_cost_lo, Birds_cost_mean, Birds_cost_hi,
                           Fish_cost_lo, Fish_cost_mean, Fish_cost_hi,
                           Mammals_cost_lo, Mammals_cost_mean,
                           Mammals_cost_hi, Total_cost_lo, Total_cost_mean,
                           Total_cost_high)
  full_carbon_scenarios <- rbind.data.frame(full_carbon_scenarios, round(temp, digits=2))
}
write.table(full_carbon_scenarios, "carbon_emissions_by_village.txt", sep="\t", row.names=FALSE)
