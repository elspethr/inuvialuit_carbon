### Estimating harvests ###

# Author: Elspeth Ready
# Contact: elspeth_ready@eva.mpg.de

# Define villages
villages = c("Aklavik", "Inuvik", "Paulatuk", "Sachs Harbour", "Tuktoyaktuk", "Ulukhaktok")

# Import harvest and edible weight data
harvdat = read.csv("Data/harvest2018_cleaned.csv") # Output of IHS_cleaning.R, not publically availible
EWdat = read.csv("Data/edibleweight_data.csv")     # Included

EW_char = c(0.65, 0.65, 1.6, 0.7, NA, 1.65)   # char weights by village, from Usher

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

# Group for market equivalents
markettype = c(rep("Mammal", 15),
               rep("Bird", 20),
               rep("Fish", 18))

# Select relevent data, and prepare for Stan               
harv_edible = harvdat[which(harvdat$SpeciesNam %in% foodsp), c("Community", "SpeciesNam", "NumHarvest")]

harv_edible$missing = ifelse(is.na(harv_edible$NumHarvest), 1, 0)
harv_edible = harv_edible[-which(harv_edible$NumHarvest==0),]     # Remove handful failed trips
harv_edible$ecotype = ecotype[match(harv_edible$SpeciesNam, foodsp)]
harv_edible$markettype = markettype[match(harv_edible$SpeciesNam, foodsp)]

##################################################### What does this do?
animinorder = sort(unique(harv_edible$SpeciesNam))
EWorder = numeric(length(animinorder))
for (i in seq_along(animinorder)) {
  if (grepl("Char", animinorder[i], fixed=TRUE) != 1) {
    EWorder[i] = EWdat$Edible_weight[which(EWdat$Name==animinorder[i])]
  }
  else {EWorder[i] = mean(EW_char, na.rm=TRUE)}
}

medharv_eco = aggregate(harv_edible$NumHarvest, by=list(harv_edible$ecotype), median, na.rm=TRUE)$x          # Note can't rerun this without reloading!
log_meanharv_eco = aggregate(log(harv_edible$NumHarvest), by=list(harv_edible$ecotype), mean, na.rm=TRUE)$x 
log_sdharv_eco = aggregate(log(harv_edible$NumHarvest), by=list(harv_edible$ecotype), sd, na.rm=TRUE)$x 
harv_edible$NumHarvest[which(is.na(harv_edible$NumHarvest))] = -1
