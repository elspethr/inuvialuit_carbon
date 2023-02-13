# Import tooniktoyook data
d = read.spss("Data/Tooniktoyok/TOONIKTOYOK_SPSS_04_03_19.sav", to.data.frame = TRUE)
d$DATE1 = as.POSIXct(as.numeric(substr(d$DATE, 1, 10)), origin="1970-01-01")

#allcost = d$FUEL_COST+d$OIL_COST+d$SUPPLIES_USE
#merged_costs = allcost
#merged_costs[is.na(merged_costs)] = (d$COST_ESTIMATE)[is.na(merged_costs)]

# Adjust harvest report for group size
d$ADJ_HARV = d$INDIV_SEW
for (i in seq_along(d$ADJ_HARV)) {
  if (is.na(d$ADJ_HARV[i])) {
    if (d$TRANSPORT[i] %in% c("SNOWMACHINE", "ATV")) {
      comp = d$NO_COMPANIONS[i]
      if (comp >= 1) {
        d$ADJ_HARV[i] = d$GROUP_SEW[i]/comp
      }
    }
    else {d$ADJ_HARV[i] = d$GROUP_SEW[i]}
  }
}

#d$wpf = d$ADJ_HARV/d$FUEL_COST
#d$wptotal = d$ADJ_HARV/merged_costs
d$gaslitres = d$FUEL_USE*4.54609 #convert imperial gallons to litres
#d$naphthalitres = d$NAPTHA_USE*4.54609
#d$oillitres = d$OIL_USE*4.54609
#d$wpl = d$ADJ_HARV/d$gaslitres
idx = which(!is.na(d$gaslitres)  & !(is.na(d$ADJ_HARV)))
