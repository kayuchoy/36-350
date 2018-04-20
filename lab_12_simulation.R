generate_data = function(n, p) {
  standard.norm.covar = rnorm(n * p, mean = 0, sd = 1)
  covariates = matrix(standard.norm.covar, nrow = n, ncol = p)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff) {
  lm = lm(responses ~ covariates)
  coefs = summary(lm)$Coefficients
  pvals = coefs[3]
  retain = covariates[, which(pvals <= cutoff)]
  if (length(retain) == 0) {
    return("")
  } else {
    return(lm(responses ~ retain))
  }
}
