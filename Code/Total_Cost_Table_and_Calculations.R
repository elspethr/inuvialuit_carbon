### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Table 2 (total cost)
HPDI_temp = matrix(unlist(lapply(cost, HPDI, 0.90)), ncol=2, byrow=TRUE)
Table2 = cbind.data.frame(mean=unlist(lapply(cost, mean)),
                          sd=unlist(lapply(cost, sd)),
                          lo=HPDI_temp[,1], hi=HPDI_temp[,2])
rownames(Table2) = groupnames
write.table(round(Table2, 0), "table2.txt", sep="\t")

# total cost
print("Total cost")
print(Table2[4,1])

# total cost per kg 
print("Cost per kg")
print(Table2$mean/Table1$mean)

# total cost per beneficiary
print("Cost per beneficiary")
print(Table2[4,1]/2767)

# total cost per beneficiary, by community
comCADtotal = unlist(lapply(cost_vill, mean))
print("Cost per beneficiary by community")
print(comCADtotal/c(bene2021, 2767))
