### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# Table 1 (edible weight)
HPDI_temp = apply(samps$harvest_est[,7,], 2, HPDI, 0.90)
Table1 = cbind.data.frame(mean=apply(samps$harvest_est[,7,], 2, mean),
                sd=apply(samps$harvest_est[,7,], 2, sd),
                lo=HPDI_temp[1,], hi=HPDI_temp[2,])
rownames(Table1) = groupnames
write.table(round(Table1, 0), "table1.txt", sep="\t")

# total harvest with error
Table1[4,]

# harvests for caribou, broad whitefish, muskox and inconnu
caribouEW = samps$EW_est[,which(harv_edible$ecotype=="Caribou")]
sum(apply(caribouEW, 2, mean))
sum(apply(caribouEW, 2, sd))
muskoxEW = samps$EW_est[,which(harv_edible$ecotype=="Muskox")]
sum(apply(muskoxEW, 2, mean))
sum(apply(muskoxEW, 2, sd))
bwEW = samps$EW_est[,which(harv_edible$SpeciesNam=="Whitefish - Broad")]
sum(apply(bwEW, 2, mean))
sum(apply(bwEW, 2, sd))
inconnuEW = samps$EW_est[,which(harv_edible$SpeciesNam=="Inconnu")]
sum(apply(inconnuEW, 2, mean))
sum(apply(inconnuEW, 2, sd))
snowgooseEW = samps$EW_est[,which(harv_edible$SpeciesNam=="Goose - Snow")]
sum(apply(snowgooseEW, 2, mean))
mooseEW = samps$EW_est[,which(harv_edible$SpeciesNam=="Moose")]
sum(apply(mooseEW, 2, mean))
