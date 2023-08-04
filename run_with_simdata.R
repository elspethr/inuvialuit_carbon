### Load libraries ###
library(foreign)
library(rlang)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)
library(reshape2)
library(ggplot2)


### Set working directory ###
#setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Carbon\\inuvialuit_carbon")
setwd("~/repos/inuvialuit_carbon")

### Load some needed info ###
villages = c("Aklavik", "Inuvik", "Paulatuk", "SachsHarbour","Tuktoyaktuk", "Ulukhaktok")

### Source needed functions ###
source("Code/Functions.R")

### Estimation of emissions (per kg) ###
source("Code/Calculate_Emissions.R")

### Run robustness checks around empirical parameter set ###
#source("Code/Robustness_Checks.R")

### Source alternative functions for other simulation approach ###
source("Code/Functions_alternative.R")

### Run robustness checks around empirical parameter set for alternative approach ###
source("Code/Robustness_Checks_alternative.R")

#currently running basic version of alternative (OK!)
#then run without species dist 
#rerun with species dist (bc zeros were in error)
#then figure out the log normal scale
#then run the 4 sims again
#then run the models again
#redo figs and tables
#update text
#update supp info