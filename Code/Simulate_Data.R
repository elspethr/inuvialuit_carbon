# Code to produce simulated data for testing the model

# IHS data (i.e., EW from harvest events)

# species
foodsp = c("Caribou - Barren Ground", "Caribou - Bluenose", 
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
nsp = length(foodsp)

# Species types for harvest distributions
ecotype = c("Caribou", "Caribou", "Caribou", "Caribou", 
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
as.numeric(ecotype)

# Group for market equivalents
markettype = c(rep("Mammal", 15),
               rep("Bird", 20),
               rep("Fish", 18))

# assignment to a community (1-6)
community <- c(1:6)

# make a data frame to store our pretend data
nsim <- 3000
simIHSdat <- data.frame(sp=numeric(nsim), ecotype=numeric(nsim), 
                        community=numeric(nsim), harv_meas=numeric(nsim))

# choose a species
simIHSdat$sp <- sample(1:nsp, nsim, replace=TRUE)
# retrieve its ecotype
simIHSdat$ecotype <- ecotype[]

  
# choose a community
# simulate a harvest (each ecotype needs a lognormal distribution of harvest sizes with mean and sd and error according to the scale factor)
}

# Tooniktoyok data (i.e., fuel use by kg)
# observed trips
# simulate success or no success
# if no success, simulate fuel use
# if success, simulate kg return, calculate fuel use from linear model 


# Other inputs:
# EW, market costs, market emissions, fuel costs, fuel emissions are all fixed
