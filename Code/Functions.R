### determine scaling factor for deheaping ###
calc_sd = function(median, sigma) {
  # lognormal sigma
  # log(median) is lognormal mu
  return(sqrt((exp(sigma^2) - 1) * exp((2 * log(median)) + sigma^2)))
}
