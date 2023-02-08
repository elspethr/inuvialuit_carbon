### Estimating fuel use ###

# Author: Elspeth Ready
# Contact: elspeth_ready@eva.mpg.de

### Get data ###

setwd("~/Dropbox/Carbon_estimation/Analysis/")

villages <- c("Aklavik", "Inuvik", "Paulatuk", "Sachs Harbour", 
              "Tuktoyaktuk", "Ulukhaktok")

#import harvest and edible weight data
harvdat <- read.csv("harvest2018_cleaned.csv") #output of IHS_cleaning.R
EWdat <- read.csv("edibleweight_data.csv")
EW_char <- c(0.65, 0.65, 1.6, 0.7, NA, 1.65) #char weights by village, from Usher
foodsp <- c("Caribou - Barren Ground", "Caribou - Bluenose", 
            "Caribou - Peary", "Caribou - Porcupine", 
            "Caribou - Union Dolphin", "Caribou - Woodland",
            "Whale - Beluga", "Seal - Ringed", "Bear - Polar", "Moose", 
            "Muskox", "Beaver", "Muskrat", "Hare - Arctic", "Hare - Snowshoe",
            "Brant", "Goose - Canada", "Goose - Greater White",
            "Goose - Ross", "Goose - Snow","Ptarmigan - Rock",
            "Ptarmigan - Willow", "Duck - Unknown",
            "Canvasback", "Eider - King", "Eider - Common", 
            "Mallard", "Merganser - Common", 
            "Scoter - Black", "Teal", "Wigeon - American", 
            "Swan - Trumpeter",  "Swan - Tundra", 
            "Loon - Common", "Crane - Sandhill",
            "Char - Arctic", "Char - Dolly Varden", "Char - Land Locked",
            "Trout - Lake", "Pike - Northern", 
            "Whitefish - Broad", "Whitefish - Lake",
            "Inconnu", "Cisco - Arctic", "Cisco - Least", 
            "Cod - Greenland", "Cod - Saffron", "Flounder", 
            "Sculpin - Fourhorn", "Burbot",  
            "Salmon", "Smelt",  "Herring - Pacific")
nsp <- length(foodsp)
harv_edible <- harvdat[which(harvdat$SpeciesNam %in% foodsp), 
                       c("Community", "SpeciesNam", "NumHarvest")]

#species types for harvest distributions
ecotype <- c("Caribou", "Caribou", "Caribou", "Caribou", 
                 "Caribou", "Caribou - Woodland",
                 "Whale - Beluga", "Seal - Ringed", "Bear - Polar", "Moose", 
                 "Muskox", "Beaver", "Muskrat", "Hare", "Hare",
                 "Goose", "Goose", "Goose",
                 "Goose", "Goose","Ptarmigan",
                 "Ptarmigan", "Duck",
                 "Duck", "Eider", "Eider", 
                 "Duck", "Duck", 
                 "Duck", "Duck", "Duck", 
                 "Swan",  "Swan", 
                 "Loon", "Crane",
                 "Char", "Char", "Char",
                 "Trout - Lake", "Pike - Northern", 
                 "Inc/White", "Inc/White",
                 "Inc/White", "Cisco", "Cisco", 
                 "Cod - Greenland", "Cod - Saffron", "Flounder", 
                 "Sculpin - Fourhorn", "Burbot",  
                 "Char", "Smelt",  "Herring - Pacific")

#group for market equivalents
markettype <- c(rep("Mammal", 15),
             rep("Bird", 20),
             rep("Fish", 18))

#process for stan
harv_edible$missing <- ifelse(is.na(harv_edible$NumHarvest), 1, 0)
harv_edible <- harv_edible[-which(harv_edible$NumHarvest==0),] #remove handful failed trips
harv_edible$ecotype <- ecotype[match(harv_edible$SpeciesNam, foodsp)]
harv_edible$markettype <- markettype[match(harv_edible$SpeciesNam, foodsp)]
animinorder <- sort(unique(harv_edible$SpeciesNam))
EWorder <- numeric(length(animinorder))
for (i in seq_along(animinorder)) {
  if (grepl("Char", animinorder[i], fixed=TRUE) != 1) {
    EWorder[i] <- EWdat$Edible_weight[which(EWdat$Name==animinorder[i])]
  }
  else {EWorder[i] <- mean(EW_char, na.rm=TRUE)}
}
medharv_eco <- aggregate(harv_edible$NumHarvest, by=list(harv_edible$ecotype), median, na.rm=TRUE)$x #note can't rerun this without reloading!
log_meanharv_eco <- aggregate(log(harv_edible$NumHarvest), by=list(harv_edible$ecotype), mean, na.rm=TRUE)$x 
log_sdharv_eco <- aggregate(log(harv_edible$NumHarvest), by=list(harv_edible$ecotype), sd, na.rm=TRUE)$x 
harv_edible$NumHarvest[which(is.na(harv_edible$NumHarvest))] <- -1

#import tooniktoyook data
library(foreign)

d <- read.spss("Tooniktoyok/TOONIKTOYOK_SPSS_04_03_19.sav", to.data.frame = TRUE)
d$DATE1 <- as.POSIXct(as.numeric(substr(d$DATE, 1, 10)), origin="1970-01-01")

#allcost <- d$FUEL_COST+d$OIL_COST+d$SUPPLIES_USE
#merged_costs <- allcost
#merged_costs[is.na(merged_costs)] <- (d$COST_ESTIMATE)[is.na(merged_costs)]

#adjust harvest report for group size
d$ADJ_HARV <- d$INDIV_SEW
for (i in seq_along(d$ADJ_HARV)) {
  if (is.na(d$ADJ_HARV[i])) {
    if (d$TRANSPORT[i] %in% c("SNOWMACHINE", "ATV")) {
      comp <- d$NO_COMPANIONS[i]
      if (comp >= 1) {
        d$ADJ_HARV[i] <- d$GROUP_SEW[i]/comp
      }
    }
    else {d$ADJ_HARV[i] <- d$GROUP_SEW[i]}
  }
}
#d$wpf <- d$ADJ_HARV/d$FUEL_COST
#d$wptotal <- d$ADJ_HARV/merged_costs
d$gaslitres <- d$FUEL_USE*4.54609 #convert imperial gallons to litres
#d$naphthalitres <- d$NAPTHA_USE*4.54609
#d$oillitres <- d$OIL_USE*4.54609
#d$wpl <- d$ADJ_HARV/d$gaslitres
idx <- which(!is.na(d$gaslitres)  & !(is.na(d$ADJ_HARV)))

### Calculate emissions scenarios ###

#kgs per litre gasoline emissions
gpl <- ((8.78*1000) + (0.38*25) + (0.08*298))/(3.78541*1000)
#weight to vol for gasoline
kgtol <- 1.3353

#note: above are stationary combustion values; mobile values differ for boats and other vehicles, may need to consider difference scenarios in future

#rail distances (for fuel)
traindists <- rep(1300, 6)
bargedists <- c(1600, 1600, 2300, 2200, 1800, 2400)
roaddists <- c(86, 0, 0, 0, 0, 0)
low_rail <- traindists*25 + bargedists*25 + roaddists*70
high_rail <- traindists*60 + bargedists*60 + roaddists*190
rail_emissions <- cbind.data.frame(villages, low=low_rail/(1000*1000), 
                              high=high_rail/(1000*1000))
rail_emissions_litre <- rail_emissions
rail_emissions_litre[,2:3] <- rail_emissions_litre[,2:3]/kgtol
rail_emissions_litre[,2:3] <- rail_emissions_litre[,2:3]+gpl

#barge transport scenarios
roadHR <- c(1136,1050,1050,1050,1050,1050)
barge <- c(1600,1600,2300,2200,1800,2400)
low_barge <- roadHR*0.00007 + barge*0.000025
high_barge <- roadHR*0.00019 + barge*0.00006
barge_emissions <- cbind.data.frame(villages, low=low_barge, high=high_barge)
  
#food mail transport scenarios
roadINU <- c(3175,3175,3175,3175,3328,3175)
air <- c(55,0,400,512,0,675)
low_FM <- roadINU*0.00007 + air*0.0008
high_FM <- roadINU*0.00019 + air*0.002
FM_emissions <- cbind.data.frame(villages, low=low_FM, high=high_FM)

#meat emissions
beef <- 20.69
pork <- 4.68
beefpork <- (beef+pork)/2
chicken <- 2.69
whitefish <- 3.4
salmonid <- 3.47
fish <- whitefish*.7 + salmonid*.3

foods <- data.frame(rep(chicken,6), rep(fish,6), rep(beefpork,6))

market_emissions <- array(data=NA, dim=c(6,4,3))
market_emissions[,1,] <- as.matrix(barge_emissions[,2]+foods)
market_emissions[,2,] <- as.matrix(barge_emissions[,3]+foods)
market_emissions[,3,] <- as.matrix(FM_emissions[,2]+foods)
market_emissions[,4,] <- as.matrix(FM_emissions[,3]+foods)

#food costs (see Ready 2021)
beefporkcost <- c(31.15, 19.26, 30.59, 20.62, 22.70, 24.87)
chickencost <- c(13.91, 15.08, 12.59, 11.20, 12.36, 15.72)
frozenfillets <- c(29.78, 21.36, 17.31, 33.70, 33.62, 41.65) 
market_costs <- cbind(chickencost, frozenfillets, beefporkcost)

### determine scaling factor for deheaping ###
calc_sd <- function(median, sigma) {
  # lognormal sigma
  # log(median) is lognormal mu
  sqrt((exp(sigma^2) - 1) * exp((2 * log(median)) + sigma^2))
}

pdf("median_sd_lognormal.pdf", pointsize=10, width=8, height=5)
curve(calc_sd(x, 0.4), xlab = "harvest size", ylab = "sd heaping error", 
      from = 1, to = 500, main="Median and standard deviation in a log normal distribution")
axis(2, at = seq(10, 50, 10), labels = rep("", length(seq(10, 50, 10))))
axis(2, at = seq(50, 300, 50), labels = rep("", length(seq(50, 300, 50))))
abline(h = seq(10, 50, 10), col = "gray", lty = 2, lwd=0.5)
abline(h = seq(50, 250, 50), col = "gray", lty = 2, lwd=0.5)
curve(calc_sd(x, 0.4), add = TRUE)
curve(calc_sd(x, 0.3), add = TRUE)
curve(calc_sd(x, 0.2), add = TRUE)
curve(calc_sd(x, 0.1), add = TRUE)
curve(calc_sd(x, 0.05), add = TRUE)
text(rep(498, 5), c(16, 40, 95, 150, 215), cex=0.8,
     labels=sapply(c(0.05, 0.1, 0.2, 0.3, 0.4), function(x) as.expression(substitute(sigma == B,
                                                                                     list(B = as.name(x))))))
dev.off()

### THE MODEL ###

library(rethinking)

N <- length(harv_edible$NumHarvest)
x = 0.15 # scaling factor
dat3 <- list(EW = d$ADJ_HARV[idx],
             Fuel = d$gaslitres[idx],
             hunter = as.numeric(as.factor(d$HUNTER[idx])), 
             community = as.numeric(as.factor(harv_edible$Community)),
             harv_meas = harv_edible$NumHarv,
             EW_miss = harv_edible$missing,
             species = as.numeric(as.factor(harv_edible$SpeciesNam)),
             ecotype = as.numeric(as.factor(harv_edible$ecotype)),
             markettype = as.numeric(as.factor(harv_edible$markettype)),
             N = length(harv_edible$NumHarvest),
             log_meanharv_eco = log_meanharv_eco,
             log_sdharv_eco = log_sdharv_eco,
             Nsp = length(unique(as.numeric(as.factor(harv_edible$SpeciesNam)))),
             tripsperc = as.numeric(table(harv_edible$Community)),
             Necotype = length(unique(as.numeric(as.factor(harv_edible$ecotype)))),
             spEW=c(EWorder, 0.65, 0.65, 1.6, 0.7, 1, 1.65), 
             scale_factor_error=x,
             market_costs = market_costs,
             market_emissions = market_emissions, #array of carbon scenarios
             fuel_cost = 1.76,
             fuel_emissions = rail_emissions_litre[,2:3])

setwd("../../../repos/inuvialuit_carbon")

harvm3 <- stan(file = "carbon_model.stan", data = dat3, 
               control=list(adapt_delta=0.99, max_treedepth=20), 
               iter=4000, chains=3, seed=4492)
# sometimes a MCMC proposal is rejected; 
# but they are accepted often enough

#save samps for working without rerunning model
samps <- extract.samples(harvm3)
str(samps)

#small version for testing 
small_samps <- extract.samples(harvm3, n=100)
write.csv(small_samps, "posterior_samples_100.csv")

#read in samps if not loaded
#samps <- read.csv(...)

plot(samps$EW_est[,1], type="l") #plot a few to check mixing
plot(samps$EW_est[,1697], type="l") 
plot(samps$market_cost_est[,7,2], type="l") 
plot(samps$fuel_total_cost[,7], type="l") 

#output harvest estimates for sanity check
sim_90lo <- sim_90hi <- numeric(N)
for (i in 1:N) {
  HPDI_perh <- HPDI(samps$sim_harv[,i], 0.90)
  sim_90lo[i] <- HPDI_perh[1]
  sim_90hi[i] <- HPDI_perh[2]
}
write.csv(cbind.data.frame(species=harv_edible$SpeciesNam, report=dat3$harv_meas,
                           mean=apply(samps$sim_harv, 2, mean), 
                           loHPDI=sim_90lo, hiHDPI=sim_90hi), 
          "simulated_harvs.csv")

### extract the results we want ###

#pop stats
pop <- c(653, 3434, 309, 110, 980, 453)
bene2021 <- c(282, 1227, 236, 85, 642, 295)

groupnames <- c("Birds", "Fish", "Mammals", "Total")

## Main text ##

# Figure 2

library(viridis)
library(scales)
library(rphylopic)

cols <- (viridis(4))[c(2,3,4,1)]

caribou <- image_data("e6e864fd-8e3d-435f-9db3-dc6869c589f1", size = "thumb")[[1]]
char <- image_data("68304786-f291-4115-9ccd-ead068ba6f19", size = "thumb")[[1]]
goose <- image_data("7b8fb3d4-0cac-4552-8cd1-bd493b7de679", size="thumb")[[1]]

pdf("Figure2.pdf", height=3.5, width=6, pointsize=9)
par(mfrow=(c(3,1)), mar=c(3,4,3,1))
plot(density(samps$harvest_est[,7,1]/1000), ylab="Density", type="l",
     main="(a) Edible weight (kg)", xlab="", ylim=c(0,1.2), 
     xlim=c(0, 135), col=cols[1])
for (i in 1:4) {
  dens <- density(samps$harvest_est[,7,i]/1000)
  polygon(dens$x, dens$y, col = alpha(cols[i], 0.4), border=cols[i])
}
plot(density(samps$market_cost_est[,7,1]/1000000), ylab="Density", 
     main="(b) Replacement value (million $)", xlab="", ylim=c(0,80), 
     xlim=c(0, 3.6), col=cols[1])
for (i in 1:4) {
  dens <- density(samps$market_cost_[,7,i]/1000000)
  polygon(dens$x, dens$y, col = alpha(cols[i], 0.4), border=cols[i])
}
plot(density(samps$carbon_cost_est[,7,1,1]/1000), ylab="Density", 
     main=expression(bold(paste("(b) ", CO[2], " emitted by replacements (tonnes)"))), 
     xlab="", ylim=c(0,0.45), 
     xlim=c(0, 1200), col=cols[1])
for (i in 1:4) {
  for (j in 1:4) {
    dens <- density(samps$carbon_cost_[,7,i,j]/1000)
    polygon(dens$x, dens$y, col = alpha(cols[i], j^1.5/10), border=cols[i])
  }
}
dev.off()

# lines from samples drawn from posterior??
pdf("Figure2_lines.pdf", height=4, width=5, pointsize=8)
par(mar=c(1,4,3,1), cex.main=0.9)
nf <- layout(matrix(c(1,2,3,4), ncol=4, byrow=TRUE), 
  widths=c(2,2,2,1))
plot(density(samps$harvest_est[,7,1]/1000)$y, 
     density(samps$harvest_est[,7,1]/1000)$x, xlab="", type="n", xaxt="n",
     main="(a) Edible weight", ylab="Tonnes", xlim=c(0,1), 
     ylim=c(0, 135))
for (i in 1:4) {
    pullasamp <- sample(1:6000, 1500)
    abline(h=samps$harvest_est[pullasamp,7,i]/1000, col=alpha(cols[i], 0.07))}
box()
plot(density(samps$market_cost_est[,7,1]/1000000)$y/localmax, 
     density(samps$market_cost_est[,7,1]/1000000)$x, xlab="", type="n", xaxt="n",
     main="(b) Replacement value", ylab="Million $", xlim=c(0,100), 
     ylim=c(0, 3.6))
for (i in 1:4) {
  pullasamp <- sample(1:6000, 1500)
  abline(h=samps$market_cost_est[pullasamp,7,i]/1000000, col=alpha(cols[i], 0.07))
}
box()
plot(density(samps$carbon_cost_est[,7,1,1]/1000)$y, 
     density(samps$carbon_cost_est[,7,1,1]/1000)$x, xlab="", type="n",  xaxt="n",
     main=expression(bold(paste("(c) ", CO[2], " emitted by replacements"))), 
     ylab="Tonnes", xlim=c(0,1000), ylim=c(0, 1200))
for (i in 1:4) {
  for (j in 1:4) {
    pullasamp <- sample(1:6000, 1500)
    abline(h=samps$carbon_cost_est[pullasamp,7,i,j]/1000, col=alpha(cols[i], 0.07))
  }
}
box()
par(mar=c(1,0,3,0))
plot(1:1000/6, 1:1000, type="n", axes=FALSE, ylab="", xlab="")
add_phylopic_base(goose, x=23, y = 50, ysize=64,
                  alpha = 1, col = cols[1])
text(75, 50, "Birds", adj=0)
add_phylopic_base(char, x=23, y = 120, ysize=64,
                  alpha = 1, col = cols[2])
text(75, 120, "Fish", adj=0)
add_phylopic_base(caribou, x=30, y = 200, ysize=76,
                  alpha = 1, col = cols[3])
text(75, 200, "Mammals", adj=0)
add_phylopic_base(goose, x=45, y = 275, ysize=38,
                  alpha = 1, col = cols[4])
add_phylopic_base(caribou, x=20, y = 300, ysize=42,
                  alpha = 1, col = cols[4])
add_phylopic_base(char, x=10, y = 260, ysize=34,
                  alpha = 1, col = cols[4])
text(75, 275, "Total", adj=0)
dev.off()

# Table 1 (edible weight)
HPDI_temp <- apply(samps$harvest_est[,7,], 2, HPDI, 0.90)
Table1 <- cbind.data.frame(type=groupnames,
                mean=apply(samps$harvest_est[,7,], 2, mean),
                sd=apply(samps$harvest_est[,7,], 2, sd),
                lo=HPDI_temp[1,], hi=HPDI_temp[2,])
Table1

# total harvest with error
Table1[4,]

# harvests for caribou, broad whitefish, muskox and inconnu
caribouEW <- samps$EW_est[,which(harv_edible$ecotype=="Caribou")]
sum(apply(caribouEW, 2, mean))
muskoxEW <- samps$EW_est[,which(harv_edible$ecotype=="Muskox")]
sum(apply(muskoxEW, 2, mean))
bwEW <- samps$EW_est[,which(harv_edible$SpeciesNam=="Whitefish - Broad")]
sum(apply(bwEW, 2, mean))
inconnuEW <- samps$EW_est[,which(harv_edible$SpeciesNam=="Inconnu")]
sum(apply(inconnuEW, 2, mean))
snowgooseEW <- samps$EW_est[,which(harv_edible$SpeciesNam=="Goose - Snow")]
sum(apply(snowgooseEW, 2, mean))
mooseEW <- samps$EW_est[,which(harv_edible$SpeciesNam=="Moose")]
sum(apply(mooseEW, 2, mean))

# harvest per beneficiary
Table1[4,2]/2767

# harvest per beneficiary by community
comkgtotal <- apply(samps$harvest_est[,,4], 2, mean)
comkgtotal/c(bene2021, 2767)

# Table 2 (total cost)
HPDI_temp <- apply(samps$market_cost_est[,7,], 2, HPDI, 0.90)
Table2 <- cbind.data.frame(type=groupnames,
                           mean=apply(samps$market_cost_est[,7,], 2, mean),
                           sd=apply(samps$market_cost_est[,7,], 2, sd),
                           lo=HPDI_temp[1,], hi=HPDI_temp[2,])
Table2

# total cost
Table2[4,2]

# total cost per kg 
Table2$mean/Table1$mean

# total cost per beneficiary
Table2[4,2]/2767

# total cost per beneficiary, by community
comCADtotal <- apply(samps$market_cost_est[,,4], 2, mean)
comCADtotal/c(bene2021, 2767)

# Table 3 (carbon emissions)
Table3 <- data.frame(type=groupnames)
for (i in 1:4) {
  HPDI_temp <- apply(samps$carbon_cost_est[,7,,i], 2, HPDI, 0.90)
  Table3 <- cbind.data.frame(Table3,
                           lo=HPDI_temp[1,], 
                           mean=apply(samps$carbon_cost_est[,7,,i], 2, mean),
                           hi=HPDI_temp[2,])
}
Table3

# total emissions 
Table3[4,c(2, 13)] #lo-high ranges of HDPIs (???)
Table3[4,c(3, 6, 9, 12)] #scenario means

# emissions per kg food
Table3[4,c(3, 6, 9, 12)]/Table1[4,2]

# emissions per beneficiary
Table3[4,c(3, 6, 9, 12)]/2767

# slope of fuel regression
mean(samps$b)
sd(samps$b)

# prob failed trip
mean(samps$theta)
sd(samps$theta)

# input gasoline (l)
input <- apply(samps$fuel_obs_est, 2, mean)
input

# n failed trips
apply(samps$nzero, 2, mean)

# additional gas (l)
apply(samps$fuel_zero_est, 2, mean)

#total gas (l)
totallitres <- apply(samps$fuel_total, 2, mean)

# total input gasoline ($)
totalgascost <- apply(samps$fuel_total_cost, 2, mean)

# total input gasoline (CO2)
lo_gas_emissions <- apply(samps$fuel_total_emissions[,,1], 2, mean) #lo barge
high_gas_emissions <- apply(samps$fuel_total_emissions[,,2], 2, mean) #high barge

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

## SI ##

# per capita harvest in diff villages
cbind(villages, comkgtotal[1:6]/bene2021)
(comkgtotal[1:6]/bene2021)[3]/0.6 #adjusted for sampling?

# additional subsidy for amount harvestes
# Aklavik, Paulatuk, Sachs Harbour, and Ulukhaktok in 2020 were $5.75, $4.95, $7.25, $5.65 
extrasubsidy <- comkgtotal[1:6]*c(5.75, 0, 4.95, 7.25, 0, 5.65)
comCADtotal+c(extrasubsidy, sum(extrasubsidy))


# Table S7
pars <- c("Theta (prob. failed trip)", "Fuel intercept - successful", "Fuel slope - successful", "Standard deviation, linear estimator", "Fuel intercept - failed trip", "Fuel sd - failed trip")
mean <- c(mean(samps$theta), mean(samps$a), mean(samps$b), 
          mean(samps$phi[,1]), mean(samps$a2), mean(samps$phi[,2]))
sds <- c(sd(samps$theta), sd(samps$a), sd(samps$b), 
          sd(samps$phi[,1]), sd(samps$a2), sd(samps$phi[,2]))

TableS7 <- cbind.data.frame(Parameter=pars, Mean=mean, SD=sds)
