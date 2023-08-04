### Checking model performance on simulated data ###

## First check around fuel on number of encounters
Q = 10
animals = make_animals()
results = array(NA,c(Q,Q,4,4))

sweep = seq(-3, 2, length.out=Q)
sweep2 = seq(0.01, 2.5, length.out=Q)


for(q in 1:Q){
  for(q2 in 1:Q){
    global_parameters = list(
      fuel = c(250, 0.20, 6), #scalar, u, v
      fuel_encounters = c(sweep[q], 0.8, sweep2[q2]), #c(0.21, 0.8, 1.146), 
      #(intercept and slope for mean), sd
      fuel_success = c(0.9, 0.005), #intercept, slope of logistic reg
      villages = c( "Akl", "Inuvik", "Paul", "Sachs", "Tuk", "Ulu"),
      mode = "All")
    results[q,q2,,] = iToraTor(animals=animals, parameters=global_parameters)
  }
}


# Plot Mean
A = results[,,2,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Enc. SD.", y="Fuel Enc. Int.", title="Matrix") 

ggsave("FuelEncounters_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI 
A = results[,,3,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Enc. SD.", y="Fuel Enc. Int.", title="Matrix") 

       ggsave("FuelEncounters_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI
A = results[,,4,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Enc. SD.", y="Fuel Enc. Int.", title="Matrix") 

ggsave("FuelEncounters_H.pdf", bob3, width = 6, height = 6)

       
# Plot Mean (HARVEST)
A = results[,,2,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD. ", y="Fuel Enc. Int. ", title="Matrix") 

ggsave("FuelEncounters_M_harvest.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI (HARVEST)
A = results[,,3,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD. ", y="Fuel Enc. Int. ", title="Matrix") 

ggsave("FuelEncounters_L_harvest.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI (HARVEST)
A = results[,,4,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD.  ", y="Fuel Enc. Int.  ", title="Matrix") 

ggsave("FuelEncounters_H_harvest.pdf", bob3, width = 6, height = 6)       
       
       

## Next check around fuel on successes
Q = 10
animals = make_animals()
results = array(NA,c(Q,Q,4,4))

sweep = seq(-2, 2, length.out=Q)
sweep2 = seq(-0.015, 0.015, length.out=Q)

for(q in 1:Q){
  for(q2 in 1:Q){
    global_parameters = list(
      fuel = c(250, 0.20, 6), #scalar, u, v
      fuel_encounters = c(0.21, 0.8, 1.146), #intercept and slope for mean, sd
      fuel_success = c(sweep[q], sweep2[q2]), #intercept, slope of logistic reg
      villages = c( "Akl", "Inuvik", "Paul", "Sachs", "Tuk", "Ulu"),
      mode = "All")
    results[q,q2,,] = iToraTor(animals=animals, parameters=global_parameters)
  }
}

# Plot Mean
A = results[,,2,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

       ggsave("FuelSuccess_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI
A = results[,,3,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

       ggsave("FuelSuccess_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI
A = results[,,4,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

ggsave("FuelSuccess_H.pdf", bob3, width = 6, height = 6)

       
# Plot Mean (HARVEST)
A = results[,,2,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
 geom_tile(aes(fill=value)) + 
 scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
 labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

ggsave("FuelSuccess_M_harvest.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI (HARVEST)
A = results[,,3,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
 geom_tile(aes(fill=value)) + 
 scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
 labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

ggsave("FuelSuccess_L_harvest.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI (HARVEST)
A = results[,,4,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
 geom_tile(aes(fill=value)) + 
 scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
 labs(x="Fuel Suc. SD.", y="Fuel Suc. Int.", title="Matrix") 

ggsave("FuelSuccess_H_harvest.pdf", bob3, width = 6, height = 6)       


## Check around shape of fuel use
Q = 10
animals = make_animals()
results = array(NA,c(Q,Q,4,4))

sweep = seq(0.15, 0.85, length.out=Q)
sweep2 = seq(5, 24, length.out=Q)

for(q in 1:Q){
  for(q2 in 1:Q){
 global_parameters = list(
    fuel = c(250, sweep[q], sweep2[q2]),   #scalar, u, v
    fuel_encounters = c(0.21, 0.8, 1.146), #intercept and slope for mean, sd
    fuel_success = c(0.9, 0.005),          #intercept, slope of logistic reg
    villages = c( "Akl", "Inuvik", "Paul", "Sachs", "Tuk", "Ulu"),
    mode = "All"
  )
results[q,q2,,] = iToraTor(animals=animals, parameters=global_parameters)
}}

# Plot Mean
A = results[,,2,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Shape SD.", y="Fuel Shape Int.", title="Matrix") 

       ggsave("FuelShape_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI
A = results[,,3,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Shape SD.", y="Fuel Shape Int.", title="Matrix") 

       ggsave("FuelShape_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI
A = results[,,4,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
       geom_tile(aes(fill=value)) + 
       scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
       labs(x="Fuel Shape SD.", y="Fuel Shape Int.", title="Matrix") 

ggsave("FuelShape_H.pdf", bob3, width = 6, height = 6)

# Plot Mean (HARVEST)
A = results[,,2,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD. ", y="Fuel Enc. Int. ", title="Matrix") 

ggsave("FuelShape_M_harvest.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI (HARVEST)
A = results[,,3,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD. ", y="Fuel Enc. Int. ", title="Matrix") 

ggsave("FuelShape_L_harvest.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI (HARVEST)
A = results[,,4,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Enc. SD.  ", y="Fuel Enc. Int.  ", title="Matrix") 

ggsave("FuelShape_H_harvest.pdf", bob3, width = 6, height = 6)       

       
      
