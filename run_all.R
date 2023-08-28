#### Load libraries ###
library(foreign)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)

# Define villages
villages = c("Aklavik", "Inuvik", "Paulatuk", "Sachs Harbour", "Tuktoyaktuk", "Ulukhaktok")

### Set working directory ###
#setwd("C:\\Users\\cody_ross\\Desktop\\inuvialuit_carbon-main")
setwd("/Users/elspethready/repos/inuvialuit_carbon")

### Load special functions ###
source("Code/Functions.R")

### Estimation of emissions (per kg) ###
source("Code/Calculate_Emissions.R")

### Process data ###
source("Code/IHS_Cleaning.R")
source("Code/Process_Harvests.R")
source("Code/Process_Tooniktoyook.R")

### Run Stan model ###
source("Code/Fit_Model.R")

### Run Stan model with alt version with no species dists ###
#source("Code/Fit_Model_noecodist.R")

### Make figures ###
source("Code/Heaping_Figure.R")
source("Code/Carbon_Cost_Figure.R") 

### Make tables and in-text calculations ###
source("Code/Edible_Weight_Table_and_Calculations.R")
source("Code/Total_Cost_Table_and_Calculations.R")
source("Code/Carbon_Emissions_Table_and_Calculations.R")
source("Code/SI_Tables_and_Calculations.R")

