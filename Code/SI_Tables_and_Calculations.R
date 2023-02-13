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
