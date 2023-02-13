####################################### Load libraries
library(foreign)
library(rethinking)
library(viridis)
library(scales)
library(rphylopic)

####################################### Set working directory
setwd("C:\\Users\\pound\\Desktop\\Carbon")

####################################### Load special functions
source("Code/Functions.R")

####################################### Process data
source("Code/IHS_Cleaning.R")
source("Code/Process_Harvest.R")
source("Code/Process_Tooniktoyook.R")

####################################### Estimation of emissions
source("Code/Calculate_Emissions.R")

####################################### Run Stan model
source("Code/Fit_Model.R")

####################################### Make figures
source("Code/Heaping_Figure.R")
source("Code/Carbon_Cost_Figure.R")

####################################### Make tables and in-text calculations
source("Code/Edible_Weight_Table_and_Calculations.R")
source("Code/Total_Cost_Table_and_Calculations.R")
source("Carbon_Emissions_Table_and_Calculations.R")
source("SI_Tables_and_Calculations.R")

