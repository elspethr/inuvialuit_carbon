# Author: Elspeth Ready
# Contact: elspeth_ready@eva.mpg.de

### Get data ###
villages = c("Aklavik", "Inuvik", "Paulatuk", "SachsHarbour", 
              "Tuktoyaktuk", "Ulukhaktok")

for (vill in villages) {
  a = FALSE
  if (paste0("JS_Harvest_Point_", vill, "_2018.LAYER.0.csv") %in% list.files(path="Data/points_2018")) {
    a = TRUE
    dat1 = read.csv(paste0("Data/points_2018/JS_Harvest_Point_", vill, "_2018.LAYER.0.csv"),
                     stringsAsFactors = FALSE)
    colnames(dat1)[which(colnames(dat1)=="HarvtMonth")] = "HrvtMon"
    colnames(dat1)[which(colnames(dat1)=="IntvwDdate")] = "IntvwDate"
    colnames(dat1)[which(colnames(dat1)=="HarvtStart")] = "HrvtStart"
    colnames(dat1)[which(colnames(dat1)=="HarvtEnd")] = "HrvtEnd"
    assign(paste0(vill, "_data"), dat1)
  }
  if (paste0("JS_Harvest_", vill, "_Point_2018.LAYER.0.csv") %in% list.files("Data/points_2018")) {
    dat2 = read.csv(paste0("Data/points_2018/JS_Harvest_",  vill, "_Point_2018.LAYER.0.csv"),
                     stringsAsFactors = FALSE)
    colnames(dat2)[which(colnames(dat2)=="HarvtMonth")] = "HrvtMon"
    colnames(dat2)[which(colnames(dat2)=="IntvwDdate")] = "IntvwDate"
    colnames(dat2)[which(colnames(dat2)=="HarvtStart")] = "HrvtStart"
    colnames(dat2)[which(colnames(dat2)=="HarvtEnd")] = "HrvtEnd"
    if (a == FALSE) {assign(paste0(vill, "_data"), dat2[,names(Aklavik_data)])}
    else {
      dat3 = rbind.data.frame(dat1[,names(Aklavik_data)], dat2[,names(Aklavik_data)], 
                               stringsAsFactors = FALSE)
      assign(paste0(vill, "_data"), dat3)
    }
  }
  #write.csv(get(paste0(vill, "_data")), paste0("harvest_", vill, "_data.csv"))
}

villages[4] = "Sachs Harbour"

# Combine
harvest2018 = rbind.data.frame(Aklavik_data, Inuvik_data, Paulatuk_data, 
                                SachsHarbour_data, Tuktoyaktuk_data, Ulukhaktok_data, 
                                stringsAsFactors = FALSE)
harvest2018$ID = seq_along(harvest2018$OBJECTID)
harvest2018 = harvest2018[,c(25, 1:24)]

# Clean dates
harvest2018$IntwDate = as.Date(as.POSIXct(as.numeric(substr(as.character(harvest2018$IntvwDate),1, 10)), origin="1970-01-01"))
harvest2018$HrvtStart = as.Date(as.POSIXct(as.numeric(substr(as.character(harvest2018$HrvtStart),1, 10)), origin="1970-01-01"))
harvest2018$HrvtEnd = as.Date(as.POSIXct(as.numeric(substr(as.character(harvest2018$HrvtEnd),1, 10)), origin="1970-01-01"))

# Temp save
#write.table(harvest2018, "harvest2018_merged_notcleaned.csv", sep=",", row.names=FALSE)

### Remove bad rows ###
# Remove test rows
harvest2018 = harvest2018[!(harvest2018$ID %in% c(779, 2364, 2365, 3022, 3023, 3071)),]

# Remove Aklavik stray "O"s
harvest2018 = harvest2018[!(harvest2018$ID %in% c(28:31, 34, 49, 1188)),]
harvest2018 = harvest2018[!(harvest2018$ID %in% c(1345, 1516, 2526, 2970, 3114, 3673, 
                                                   3674, 3683, 6063:6073, 6089, 
                                                   6096, 6104, 6106, 6168, 6215, 
                                                   6252, 6372, 6499, 6538, 6569, 
                                                   6578, 6579, 6834, 6846, 6877, 
                                                   6878, 6881:6883)),]

# Fix Aklavik where it doesn't belong
harvest2018$Community[which(harvest2018$ID %in% c(1294, 1483, 1573, 1575, 1579, 
                                                  1630, 1632, 1647, 1682, 1737,
                                                  1843, 1851, 1852, 1961, 2088))] = "Inuvik"
harvest2018$Community[which(harvest2018$ID %in% c(2262, 2263))] =  "Paulatuk"
harvest2018$Community[which(harvest2018$ID %in% c(2958))] = "Sachs Harbour"
harvest2018$Community[which(harvest2018$ID %in% c(3411, 3899, 3958, 4709, 5212,
                                                  5586, 5588, 5589))] = "Tuktoyatuk"
harvest2018$Community[which(harvest2018$Community == "Tuktoyatuk")] = "Tuktoyaktuk"
harvest2018$Community[which(harvest2018$ID %in% c(6210, 6211, 6221, 6247, 6279,
                                                  6280, 6307, 6377, 6393, 6435,
                                                  6438, 6464, 6479, 6511, 6551,
                                                  6604:6608, 6610:6613, 6617,
                                                  6665, 6766, 6838, 6855, 6859))] = "Ulukhaktok"

### CLEAN SPECIES IDS ###
# Replace species name with comments so that all "Other" remaining are unknown

# Birds
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(491, 505, 564, 1324))] = "Duck - Unknown"
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(426, 464, 466, 469, 473, 476, 485, 487, 493, 507, 512, 517, 531, 533, 538, 541, 555, 6434))] = "Goose - Greater White" #speckled
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(453, 459, 632, 1329, 6754, 6852))] = "Goose - Greater White" # yellow-legs
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(3847, 6401))] = "Goose - Snow" # blue goose
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(499, 522, 565, 679, 919))] = "Wigeon - American" # assume all Widgeons are American
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(6533, 6765))] = "Eggs"

# Fish
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(2222, 2404))] = "Burbot" #"Loch/Loche" 
harvest2018$SpeciesNam[which(harvest2018$ID == 2885)] = "Salmon"
harvest2018$SpeciesNam[which(harvest2018$ID ==2393)] = "Cod - Greenland" #Rock cod
harvest2018$SpeciesNam[which(harvest2018$SpeciesNam == "Cod - Arctic")] = "Cod - Greenland"  #Arctic Cod
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(4338, 4929, 4936, 5025, 5185, 5320))] = "Smelt"
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(5152))] = "Jellyfish"
harvest2018$SpeciesNam[which(harvest2018$SpeciesNam == "Herring - Lake")] = "Cisco - Least"  #Arctic Cod

#mammals
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(56, 57, 89))] = "Hare - Snowshoe"
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(133, 1929, 1941, 1943))] = "Caribou - Bluenose"
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(170))] = "Fox - Coloured" #silver, so prob. Vulpes vulpes
harvest2018$SpeciesNam[which(harvest2018$ID %in% c(3433))] = "Other"
harvest2018$SpeciesNam[which(harvest2018$ID == 1195)] = "Mink"
harvest2018$SpeciesNam[which(harvest2018$ID ==1196)] = "Lynx"
harvest2018$SpeciesNam[which(harvest2018$ID ==1197)] = "Beaver"

# Group all our beautiful animals
cariboutypes = c("Caribou - Barren Ground", "Caribou - Bluenose", 
                  "Caribou - Peary", "Caribou - Porcupine", 
                  "Caribou - Union Dolphin", "Caribou - Woodland")

foodmammals = c("Whale - Beluga", "Seal - Bearded", "Seal - Ringed", 
                "Bear - Polar", "Bear - Grizzly", "Moose", 
                "Muskox", "Beaver", "Muskrat", "Hare - Arctic", "Hare - Snowshoe")

furmammals = c("Wolf", "Lynx", "Wolverine", "Fox - Arctic", "Fox - Coloured",
                "Marten", "Otter - River", "Mink",  "Squirrel") #assume these are not eaten

birds = c("Brant", "Goose - Canada", "Goose - Greater White",
           "Goose - Ross", "Goose - Snow","Ptarmigan - Rock","Ptarmigan - Willow", "Duck - Unknown",
           "Canvasback", "Eider - King", "Eider - Common", "Mallard", "Merganser - Common", 
           "Scoter - Black", "Teal", "Wigeon - American", "Swan - Trumpeter",  "Swan - Tundra", 
           "Loon - Common", "Crane - Sandhill", "Eggs") 

fish = c("Char - Arctic", "Char - Dolly Varden", "Char - Land Locked",
          "Trout - Lake", "Pike - Northern", "Whitefish - Broad", "Whitefish - Lake",
          "Inconnu", "Cisco - Arctic", "Cisco - Least", 
          "Cod - Greenland", "Cod - Saffron", "Flounder", "Sculpin - Fourhorn", "Burbot",  
          "Salmon", "Smelt",  "Herring - Pacific", "Jellyfish") #least cisco is lake herring!

allanimals = c(cariboutypes, foodmammals, furmammals, birds, fish)


### Clean number harvested (mostly entry errors) ###

# Where they are duplicated (HarvestNum in NumHarvest)
matches = harvest2018[which((harvest2018$NumHarvest == harvest2018$HarvestNum) & harvest2018$NumHarvest!=0), c(1, 3, 4, 11)]
length(matches$ID) #38
fixmatches = c(534, 790, 791, 792, 1442, 1929, 3743, 6120, 6402, 6600, 6865)

# where some other number (someone else's harvnum, or the spatial ID) appears in harvest num
toohigh = harvest2018[which(harvest2018$NumHarvest>=50), c(1, 3, 4, 11)]
length(toohigh$ID) #219
fixtoohigh = c(1198, 1677, 1850, 4392, 6636, 6671, 6766, 6891)

#double checking for nonsensical harvests of mammsals
toohighmammals = harvest2018[which(harvest2018$NumHarvest<100 & harvest2018$NumHarvest>10 & harvest2018$SpeciesTy==3), c(1, 3, 4, 11)]
length(toohighmammals$ID) #87 entries, no errors found that not already detected

# Replace these with NAs temporarily (for imputation later)
harvest2018$NumHarvest[which(harvest2018$ID %in% c(fixmatches, fixtoohigh))] = NA

# Fix the HarvestCod = 1 with "zero" harvest.
harvest2018$NumHarvest[which(harvest2018$HrvtCode==1 & 
                               harvest2018$SpeciesNam != "" & #assume harvested unknown #quantity
                               harvest2018$NumHarvest==0)] = NA #seems reasonable given data

harvest2018$HrvtCode[which(harvest2018$HrvtCode==1 & 
                             harvest2018$SpeciesNam == "" & #new code, for "harvested but no info" -
                             harvest2018$NumHarvest==0)] = 10  #treat like refusal?
harvest2018$HrvtCode[which(harvest2018$ID %in% c(1616:1618))] = 3 #comment "too busy"

# Fix double reporting of a beluga hunt
harvest2018 = harvest2018[-which(harvest2018$ID==1584),]
harvest2018$NumHarvest[which(harvest2018$ID==1587)] = 2

# Aklavik ID errors
harvest2018$HarvestNum[which(harvest2018$ID == 202)] = 3053
harvest2018$HarvestNum[which(harvest2018$ID == 354)] = 3217
harvest2018$HarvestNum[which(harvest2018$ID == 371)] = 6858
harvest2018$HarvestNum[which(harvest2018$ID %in% c(466, 467))] = 3201
harvest2018$HarvestNum[which(harvest2018$ID == 475)] = 3130
harvest2018$HarvestNum[which(harvest2018$ID == 505)] = 6817
harvest2018$HarvestNum[which(harvest2018$ID == 679)] = 3120
harvest2018$HarvestNum[which(harvest2018$ID == 767)] = 3388
harvest2018$HarvestNum[which(harvest2018$ID == 775)] = "UK2"
harvest2018$HarvestNum[which(harvest2018$ID %in% c(1030, 1031))] = 6823

# Inuvik
harvest2018$HarvestNum[which(harvest2018$ID == 1363)] = 542
harvest2018$HarvestNum[which(harvest2018$ID %in% c(1394, 1395))] = "UK1"
harvest2018$HarvestNum[which(harvest2018$ID == 1432)] = "UK2"
harvest2018$HarvestNum[which(harvest2018$ID == 1479)] = "UK3"
harvest2018$HarvestNum[which(harvest2018$ID == 1498)] = "UK4"
harvest2018$HarvestNum[which(harvest2018$ID == 1850)] = 20
harvest2018$HarvestNum[which(harvest2018$ID == 1521)] = "UK5"
harvest2018$HarvestNum[which(harvest2018$ID == 1588)] = 194

# Paul - no obvious errors

# Sachs
harvest2018$HarvestNum[which(harvest2018$Community=="Sachs Harbour" & 
                               harvest2018$HarvestNum == 101)] = "UK2"
harvest2018$HarvestNum[which(harvest2018$Community=="Sachs Harbour" 
                             & harvest2018$HarvestNum == 183)] = 8
harvest2018$HarvestNum[harvest2018$OBJECTID == 2958] = NA
harvest2018$HarvestNum[harvest2018$OBJECTID == 3173] = "UK1"

# Tuk
harvest2018$HarvestNum[harvest2018$ID == 3743] = 1180
harvest2018$HarvestNum[harvest2018$ID == 3978] = 1243
harvest2018$HarvestNum[harvest2018$ID == 3247] = 1038
harvest2018$HarvestNum[harvest2018$ID == 3269] = "UK1" #unique month entry
harvest2018$HarvestNum[harvest2018$ID == 3395] = "UK2"
harvest2018$HarvestNum[harvest2018$ID == 3965] = "UK3"
harvest2018$HarvestNum[which(harvest2018$Community=="Tuktoyaktuk" & harvest2018$HarvestNum == 609)] = 1049 #(either 1049 or 1019)

# Ulu
harvest2018$HarvestNum[which(harvest2018$Community=="Ulukhaktok" & harvest2018$HarvestNum == 1054)] = 49 #entry error
harvest2018$HarvestNum[which(harvest2018$Community=="Ulukhaktok" & harvest2018$HarvestNum %in% c(1067, 1068))] = 2
harvest2018$HarvestNum[harvest2018$ID == 6766] = 97

#for (vill in villages) {
#  print(vill)
#  hunters = unique(as.numeric(harvest2018$HarvestNum[harvest2018$Community==vill]))
#  print(length(hunters))
#  print(sort(hunters, na.last=TRUE))
#}

# Fix "catch and releases" before tallying harvests 
harvest2018$NumHarvest[which(harvest2018$ID==6576)] = 2
harvest2018[!which(harvest2018$ID %in% c(123, 4747:4751, 5023, 6454))] = 0

write.csv(harvest2018, "Data/harvest2018_cleaned.csv")
