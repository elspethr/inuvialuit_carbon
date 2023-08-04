### Checking model performance on simulated data ###

#theta = 0.25
#fuel_fail_alpha = 3 
#fuel_fail_sd = 1
#fuel_success_alpha = 2
#fuel_success_beta = 0.4
#fuel_success_sd = 0.8

## First check around fuel on number of encounters
Q = 10
animals = make_animals()
results = array(NA,c(Q,Q,4,4))

sweep = seq(0.1, 5, length.out=Q)
sweep2 = seq(0.1, 2, length.out=Q)


for(q in 1:Q){
  for(q2 in 1:Q){
    global_parameters = list(
      theta = 0.25,
      fuel_fail_alpha = sweep[q], 
      fuel_fail_sd = sweep2[2],
      fuel_success_alpha = 2,
      fuel_success_beta = 0.4,
      fuel_success_sd = 0.8,
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
  labs(x="Fuel Fail. SD.", y="Fuel Fail. Int.", title="Matrix") 

ggsave("alternative/Fuel_Fail_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI 
A = results[,,3,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Fail. SD.", y="Fuel Fail. Int.", title="Matrix") 

ggsave("alternative/Fuel_Fail_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI
A = results[,,4,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Fail. SD.", y="Fuel Fail. Int.", title="Matrix") 

ggsave("alternative/Fuel_fail_H.pdf", bob3, width = 6, height = 6)


# Plot Mean (HARVEST)
A = results[,,2,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Fail. SD. ", y="Fuel Fail. Int. ", title="Matrix") 

ggsave("alternative/Fuel_fail_harvest_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI (HARVEST)
A = results[,,3,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Fail. SD. ", y="Fuel Fail. Int. ", title="Matrix") 

ggsave("alternative/Fuel_fail_harvest_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI (HARVEST)
A = results[,,4,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Fail. SD.  ", y="Fuel Fail. Int.  ", title="Matrix") 

ggsave("alternative/Fuel_fail_harvest_H.pdf", bob3, width = 6, height = 6)       



## Next check around fuel on successes
Q = 10
animals = make_animals()
results = array(NA,c(Q,Q,4,4))

sweep = seq(-2, 4, length.out=Q)
sweep2 = seq(0.01, 1.2, length.out=Q)


for(q in 1:Q){
  for(q2 in 1:Q){
    global_parameters = list(
      theta = 0.25,
      fuel_fail_alpha = 3, 
      fuel_fail_sd = 1,
      fuel_success_alpha = sweep[q],
      fuel_success_beta = sweep[q2],
      fuel_success_sd = 0.8,
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
  labs(x="Fuel Success alpha", y="Fuel Success beta", title="Matrix") 

ggsave("alternative/Fuel_Success_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI 
A = results[,,3,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Success alpha", y="Fuel Success beta", title="Matrix") 

ggsave("alternative/Fuel_Success_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI
A = results[,,4,3]/results[,,1,3]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Success alpha", y="Fuel Success beta", title="Matrix") 

ggsave("alternative/Fuel_Success_H.pdf", bob3, width = 6, height = 6)


# Plot Mean (HARVEST)
A = results[,,2,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob1 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Success alpha", y="Fuel Success  beta", title="Matrix") 

ggsave("alternative/Fuel_Success_harvest_M.pdf", bob1, width = 6, height = 6)

# Plot Lower HPDI (HARVEST)
A = results[,,3,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob2 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Success alpha", y="Fuel Success beta", title="Matrix") 

ggsave("alternative/Fuel_Success_harvest_L.pdf", bob2, width = 6, height = 6)

# Plot Upper HPDI (HARVEST)
A = results[,,4,4]/results[,,1,4]
rownames(A) = sweep
colnames(A) = sweep2

longData = melt((A))

bob3 = ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_tile(aes(fill=value)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "grey90", high = "red") +
  labs(x="Fuel Success. alpha", y="Fuel Success. beta", title="Matrix") 

ggsave("alternative/Fuel_Success_H_harvest.pdf", bob3, width = 6, height = 6)      

