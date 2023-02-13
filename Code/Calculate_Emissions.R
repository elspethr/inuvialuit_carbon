### Calculate emissions scenarios ###
# Kgs per litre gasoline emissions
gpl = ((8.78*1000) + (0.38*25) + (0.08*298))/(3.78541*1000)

# Weight to vol for gasoline
kgtol = 1.3353

# Note: the above are stationary combustion values; mobile values differ for boats and other vehicles, may need to consider difference scenarios in future

# Rail distances (for fuel)
traindists = rep(1300, 6)
bargedists = c(1600, 1600, 2300, 2200, 1800, 2400)
roaddists = c(86, 0, 0, 0, 0, 0)
low_rail = traindists*25 + bargedists*25 + roaddists*70
high_rail = traindists*60 + bargedists*60 + roaddists*190

rail_emissions = cbind.data.frame(villages, 
                                  low=low_rail/(1000*1000), 
                                  high=high_rail/(1000*1000)
                                  )

rail_emissions_litre = rail_emissions
rail_emissions_litre[,2:3] = rail_emissions_litre[,2:3] / kgtol
rail_emissions_litre[,2:3] = rail_emissions_litre[,2:3] + gpl

# Barge transport scenarios
roadHR = c(1136,1050,1050,1050,1050,1050)
barge = c(1600,1600,2300,2200,1800,2400)
low_barge = roadHR*0.00007 + barge*0.000025
high_barge = roadHR*0.00019 + barge*0.00006

barge_emissions = cbind.data.frame(villages, 
                                   low=low_barge, 
                                   high=high_barge
                                   )
  
# Food mail transport scenarios
roadINU = c(3175,3175,3175,3175,3328,3175)
air = c(55,0,400,512,0,675)
low_FM = roadINU*0.00007 + air*0.0008
high_FM = roadINU*0.00019 + air*0.002

FM_emissions = cbind.data.frame(villages, 
                                low=low_FM, 
                                high=high_FM
                                )

# Meat emissions
beef = 20.69
pork = 4.68
beefpork = (beef+pork)/2
chicken = 2.69
whitefish = 3.4
salmonid = 3.47
fish = whitefish*0.7 + salmonid*0.3

foods = data.frame(rep(chicken,6), rep(fish,6), rep(beefpork,6))

market_emissions = array(data=NA, dim=c(6,4,3))
market_emissions[,1,] = as.matrix(barge_emissions[,2] + foods)
market_emissions[,2,] = as.matrix(barge_emissions[,3] + foods)
market_emissions[,3,] = as.matrix(FM_emissions[,2] + foods)
market_emissions[,4,] = as.matrix(FM_emissions[,3] + foods)

# Food costs (see Ready 2021)
beefporkcost = c(31.15, 19.26, 30.59, 20.62, 22.70, 24.87)
chickencost = c(13.91, 15.08, 12.59, 11.20, 12.36, 15.72)
frozenfillets = c(29.78, 21.36, 17.31, 33.70, 33.62, 41.65) 
market_costs = cbind(chickencost, frozenfillets, beefporkcost)

