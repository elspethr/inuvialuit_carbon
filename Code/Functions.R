### determine scaling factor for deheaping ###
calc_sd = function(median, sigma) {
  # lognormal sigma
  # log(median) is lognormal mu
  return(sqrt((exp(sigma^2) - 1) * exp((2 * log(median)) + sigma^2)))
}

### format a dataframe for inspect ###
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

### inverse logit for binomial regression ###
inv_logit = function(x){
  p = 1/(1 + exp(-x))
  p = ifelse(x == Inf, 1, p)
  return(p)
}

### rounding to nearest integer @@@
n_int_digits = function(x) {
  result = floor(log10(abs(x)))
  result[!is.finite(result)] = 0
  result
}