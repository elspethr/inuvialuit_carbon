### Extract the results we want ###
# Pop stats
pop = c(653, 3434, 309, 110, 980, 453)
bene2021 = c(282, 1227, 236, 85, 642, 295)

groupnames = c("Birds", "Fish", "Mammals", "Total")

# additional subsidy for amount harvested
# Aklavik, Paulatuk, Sachs Harbour, and Ulukhaktok in 2020 were $5.75, $4.95, $7.25, $5.65 
extrasubsidy <- comkgtotal[1:6]*c(5.75, 0, 4.95, 7.25, 0, 5.65)
print("Cost with extra subsidies")
print(comCADtotal+c(extrasubsidy, sum(extrasubsidy)))

# Table S7
pars = c("Theta (prob. failed trip)", "Fuel intercept - successful", 
         "Fuel slope - successful", "Standard deviation, linear estimator", 
         "Fuel intercept - failed trip", "Fuel sd - failed trip")
mean = round(c(mean(inv_logit(samps$theta)), mean(samps$alpha[,1]), mean(samps$alpha[,2]), 
          mean(samps$phi_alpha), mean(samps$beta), mean(samps$phi_beta)), 3)
sds = round(c(sd(inv_logit(samps$theta)), sd(samps$alpha[,1]), sd(samps$alpha[,2]), 
          sd(samps$phi_alpha), sd(samps$beta), sd(samps$phi_beta)),3) 

TableS7 = cbind.data.frame(Parameter=pars, Mean=mean, SD=sds)
write.table(TableS7, "tableS7.txt", sep="\t", row.names=FALSE)

# Additional tables with food type by village breakdowns
village <- c(villages, "Total")
types <- c("Birds", "Fish", "Mammals", "Total")

TableS8 <- TableS9 <- TableS10 <- data.frame(village=character(4*7))
x <- 1
for (i in 1:7) {
  
  j <- i
  if (i == 7) {j <- c(1:7)}
  
  for (t in 1:4) {
    
    TableS8$village[x] <- TableS9$village[x] <- TableS10$village[x] <- village[i]
    TableS8$markettype[x] <- TableS9$markettype[x] <- TableS10$markettype[x] <- types[t]
    
    k <- t
    if (t == 4) {k <- c(1,2,3,4)}
    
    idx <- which(dat3$community %in% j & dat3$markettype %in% k)
    
    wvalue <- apply(samps$harvest_weight[,idx], 1, sum)
    TableS8$HPDIlo[x] <- round(HPDI(wvalue, 0.90)[1], 0)
    TableS8$mean[x] <- round(mean(wvalue), 0)
    TableS8$HPDIhigh[x] <- round(HPDI(wvalue, 0.90)[2], 0)
    
    cvalue <- apply(samps$market_costs[,idx], 1, sum)
    TableS9$HPDIlo[x] <- round(HPDI(cvalue, 0.90)[1], 2)
    TableS9$mean[x] <- round(mean(cvalue), 2)
    TableS9$HPDIhigh[x] <- round(HPDI(cvalue, 0.90)[2], 2)
    
    evalue <- apply(samps$emissions_barge_low[,idx], 1, sum)
    TableS10$HPDIlo_bl[x] <- round(HPDI(evalue, 0.90)[1]/1000, 0)
    TableS10$mean_bl[x] <- round(mean(evalue)/1000, 0)
    TableS10$HPDIhigh_bl[x] <- round(HPDI(evalue, 0.90)[2]/1000, 0)
    
    evalue <- apply(samps$emissions_barge_high[,idx], 1, sum)
    TableS10$HPDIlo_bh[x] <- round(HPDI(evalue, 0.90)[1]/1000, 0)
    TableS10$mean_bh[x] <- round(mean(evalue)/1000, 0)
    TableS10$HPDIhigh_bh[x] <- round(HPDI(evalue, 0.90)[2]/1000, 0)
    
    evalue <- apply(samps$emissions_mail_low[,idx], 1, sum)
    TableS10$HPDIlo_ml[x] <- round(HPDI(evalue, 0.90)[1]/1000, 0)
    TableS10$mean_ml[x] <- round(mean(evalue)/1000, 0)
    TableS10$HPDIhigh_ml[x] <- round(HPDI(evalue, 0.90)[2]/1000, 0)
    
    evalue <- apply(samps$emissions_mail_high[,idx], 1, sum)
    TableS10$HPDIlo_mh[x] <- round(HPDI(evalue, 0.90)[1]/1000, 0)
    TableS10$mean_mh[x] <- round(mean(evalue)/1000, 0)
    TableS10$HPDIhigh_mh[x] <- round(HPDI(evalue, 0.90)[2]/1000, 0)
    
    x <- x+1
  }
}

write.table(TableS8, "tableS8.txt", sep="\t", row.names=FALSE)
write.table(TableS9, "tableS9.txt", sep="\t", row.names=FALSE)
write.table(TableS10, "tableS10.txt", sep="\t", row.names=FALSE)
