generate_data = function(n, p) {
  standard.norm.covar = rnorm(n * p, mean = 0, sd = 1)
  covariates = matrix(standard.norm.covar, nrow = n, ncol = p)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates, responses))
}
