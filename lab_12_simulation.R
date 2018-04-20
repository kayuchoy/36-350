generate_data = function(n, p) {
  standard.norm.covar = rnorm(n * p, mean = 0, sd = 1)
  covariates = matrix(standard.norm.covar, nrow = n, ncol = p)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff) {
  lm = lm(responses ~ covariates)
  coefs = summary(lm)$Coefficients
  pvals = coefs[, 3]
  retain = covariates[, which(pvals <= cutoff)]
  if (length(retain) == 0) {
    return("")
  } else {
    return(lm(responses ~ retain))
  }
}

run_simulation = function(n_trials, n, p, cutoff) {
  for (i in 1:n_trials) {
    rand.dat = generate_data(n, p)
    covariates = rand.dat[[1]]
    responses = rand.dat[[2]]
    lm = model_select(covariates, responses, cutoff)
    coefs = summary(lm)$Coefficients
    pvals = coefs[, 3]
    hist(pvals,
         xlab = "p value",
         ylab = "Frequency",
         main = "Histogram of p values of Covariates from Linear Regression")
  }
}