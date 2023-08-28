### Extract the results we want ###

# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Table 1 (edible weight)
HPDI_temp = matrix(unlist(lapply(weight, HPDI, 0.90)), ncol=2, byrow=TRUE)
Table1 = cbind.data.frame(mean=unlist(lapply(weight, mean)),
                sd=unlist(lapply(weight, sd)),
                lo=HPDI_temp[,1], hi=HPDI_temp[,2])
rownames(Table1) = groupnames
write.table(round(Table1, 0), "table1.txt", sep="\t")

# total harvest with error
print("Total harvest")
print(Table1[4,])

# harvests for caribou, broad whitefish, muskox and inconnu
caribouEW = samps$harvest_weight[,which(harv_edible$ecotype=="Caribou")]
print("Caribou")
print(mean(apply(caribouEW, 1, sum)))
print(sd(apply(caribouEW, 1, sum)))
muskoxEW = samps$harvest_weight[,which(harv_edible$ecotype=="Muskox")]
print("Muskox")
print(mean(apply(muskoxEW, 1, sum)))
print(sd(apply(muskoxEW, 1, sum)))
bwEW = samps$harvest_weight[,which(harv_edible$SpeciesNam=="Whitefish - Broad")]
print("Whitefish")
print(mean(apply(bwEW, 1, sum)))
print(sd(apply(bwEW, 1, sum)))
inconnuEW = samps$harvest_weight[,which(harv_edible$SpeciesNam=="Inconnu")]
print("Inconnu")
print(mean(apply(inconnuEW, 1, sum)))
print(sd(apply(inconnuEW, 1, sum)))
snowgooseEW = samps$harvest_weight[,which(harv_edible$SpeciesNam=="Goose - Snow")]
print("Snowgoose")
print(mean(apply(snowgooseEW, 1, sum)))
print(sd(apply(snowgooseEW, 1, sum)))
mooseEW = samps$harvest_weight[,which(harv_edible$SpeciesNam=="Moose")]
print("Moose")
print(mean(apply(mooseEW, 1, sum)))
print(sd(apply(mooseEW, 1, sum)))

# harvest per beneficiary
print("Harvest per beneficiary")
print(Table1[4,1]/2767)

# harvest per beneficiary by community
comkgtotal = unlist(lapply(weight_vill, mean))
print("Per beneficiary harvest by community")
print(comkgtotal/c(bene2021, 2767))
print("Adjusted estimate for Paulatuk")
print((comkgtotal[1:6]/bene2021)[3]/0.6) #adjusted for sampling?
