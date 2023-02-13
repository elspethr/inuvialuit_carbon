### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# harvest per beneficiary
Table1[4,1]/2767

# harvest per beneficiary by community
comkgtotal = apply(samps$harvest_est[,,4], 2, mean)
comkgtotal/c(bene2021, 2767)

# Table 2 (total cost)
HPDI_temp = apply(samps$market_cost_est[,7,], 2, HPDI, 0.90)
Table2 = cbind.data.frame(mean=apply(samps$market_cost_est[,7,], 2, mean),
                           sd=apply(samps$market_cost_est[,7,], 2, sd),
                           lo=HPDI_temp[1,], hi=HPDI_temp[2,])
rownames(Table2) = groupnames
write.table(round(Table2,0), "table2.txt", sep="\t", row.names=TRUE)

# total cost
Table2[4,1]

# total cost per kg 
Table2$mean/Table1$mean

# total cost per beneficiary
Table2[4,1]/2767

# total cost per beneficiary, by community
comCADtotal = apply(samps$market_cost_est[,,4], 2, mean)
comCADtotal/c(bene2021, 2767)
