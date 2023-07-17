### assessing simulation performance ###

pdf("performance_simulated_data.pdf")
par(mfrow=c(4,1), mar=c(3,4,3,1))
# how does the actual harvest compare to the estimated one

plot(density(samps$total_harvest_estimate), axes=TRUE,  ylab="", xlab="", 
     main="Total weight", xlim=c(1e5, 1.3e5), type="n")
abline(v=samps$total_harvest_estimate, col=alpha("blue", 0.25), lwd=1)
abline(v=sum(IHS_sim$true_edible_weight), col="red", lwd=2)
#abline(v=sum(IHS_sim$reported_edible_weight), col="red", lwd=2)

## fuel use successes
real_fuel_success <- sum(IHS_sim$fuel_used[which(IHS_sim$trip_successful==1)])
plot(density(samps$fuel_usage_successes), axes=TRUE,  ylab="", xlab="", 
     main="Fuel successes", type="n")
abline(v=samps$fuel_usage_successes, col=alpha("blue", 0.25), lwd=1)
abline(v=real_fuel_success, col="red", lwd=2)

## fuel use failures
real_fuel_failure <- sum(IHS_sim$fuel_used[which(IHS_sim$trip_successful==0)])
plot(density(samps$fuel_usage_failures_total), axes=TRUE,  ylab="", xlab="", 
     main="Fuel failures", type="n")
abline(v=samps$fuel_usage_failures_total, col=alpha("blue", 0.25), lwd=1)
abline(v=real_fuel_failure, col="red", lwd=2)

## total fuel use
plot(density(samps$total_fuel_usage), axes=TRUE,  ylab="", xlab="", 
     main="Total fuel usage", type="n")
abline(v=samps$total_fuel_usage, col=alpha("blue", 0.25), lwd=1)
abline(v=sum(IHS_sim$fuel_used), col="red", lwd=2)

dev.off()




### OLD ###

# Fuel model
#samps$alpha[,1] #fuel intercept (success; 2)
#samps$alpha[,2] #fuel slope (0.4)
#samps$phi_alpha #fuel error (success; 0.8)
#samps$beta # fuel intercept (failure; 3)
#samps$phi_beta # failure fuel error 
#samps$theta # failure theta

#par(mfrow=c(6,1), mar=c(3,0,3,0))
#plot(density(exp(samps$theta)), axes=TRUE, ylab="", main="theta (Probability failure)")
#abline(v=0.25, col="red")
#plot(density(samps$alpha[,1]), axes=TRUE, ylab="", main="alpha1 (Fuel intercept)")
#abline(v=2, col="red")
#plot(density(samps$alpha[,2]), axes=TRUE,  ylab="", main="alpha2 (Fuel slope)")
#abline(v=0.4, col="red")
#plot(density(samps$phi_alpha), axes=TRUE, ylab="", main="phi_alpha (Fuel error linear predictor)")
#abline(v=0.8, col="red")
#plot(density(samps$beta), axes=TRUE, ylab="", main="beta (Fuel intercept; failure)")
#abline(v=3, col="red")
#plot(density(samps$phi_beta[,2]), axes=TRUE, ylab="", main="phi_beta (Fuel error; failure)")
#abline(v=1, col="red")


## OLD tests, not working ##
# Measurement model
#mean_harv_heap <- apply(samps$harv_true, 2, median)
#mean_harv_sim <- apply(samps$sim_harv, 2, median)
#mean_fuel_est <- apply(samps$sim_harv, 2, median)

#par(mfrow=c(3,1), mar=c(2,0,3,0))
#IHS_sim$harv_error <- mean_harv_sim-IHS_sim$true_harv_sim
#hist(IHS_sim$harv_error, breaks=1000, main="Estimate minus true harvest size")
#IHS_sim$EW_error <- apply(samps$EW_est, 2, mean) - IHS_sim$EW_harv_sim
#hist(IHS_sim$EW_error, breaks=1000, main="Estimate minus true harvest (edible weight)")
#IHS_sim$fuel_error <- mean_fuel_est - IHS_sim$fuel_successful
#hist(IHS_sim$fuel_error, breaks=1000, main="Estimate minus true fuel use")

#what are the outliers
#OK so we have some problems with big fish harvests (makes sense)
#IHS_sim[which(abs(IHS_sim$harv_error)>100),]
#IHS_sim[which(abs(IHS_sim$EW_error)>100),]
#IHS_sim[which(abs(IHS_sim$fuel_error)>100),]

