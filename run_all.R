####################################### Load libraries
library(foreign)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)

####################################### Set working directory
setwd("/Users/elspethready/repos/inuvialuit_carbon")

####################################### Load special functions
source("Code/Functions.R")

####################################### Process data
source("Code/IHS_Cleaning.R")
source("Code/Process_Harvests.R")
source("Code/Process_Tooniktoyook.R")

####################################### Estimation of emissions (per kg)
source("Code/Calculate_Emissions.R")

####################################### Run Stan model
source("Code/Fit_Model.R")

####################################### Make figures
source("Code/Heaping_Figure.R")
source("Code/Carbon_Cost_Figure.R")

####################################### Make tables and in-text calculations
source("Code/Edible_Weight_Table_and_Calculations.R")
source("Code/Total_Cost_Table_and_Calculations.R")
source("Code/Carbon_Emissions_Table_and_Calculations.R")
source("Code/SI_Tables_and_Calculations.R")

