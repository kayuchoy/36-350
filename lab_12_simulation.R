generate_data = function(n, p) {
  standard.norm.covar = rnorm(n * p, mean = 0, sd = 1)
  covariates = matrix(standard.norm.covar, nrow = n, ncol = p)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates, responses))
}

model_select = function(covariates, responses, cutoff) {
  lm = lm(responses ~ covariates)
  coefs = summary(lm)[4]
  pvals = coefs[[1]][, 3]
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
    coefs = summary(lm)[4]
    pvals = coefs[[1]][, 3]
    saveRDS(pvals, file = "pvals.rds")
    make_plot("pvals.rds")
  }
}

make_plot = function(datapath) {
  pvals = load(datapath)
  hist(pvals,
       xlab = "p value",
       ylab = "Frequency",
       main = "Histogram of p values of Covariates from Linear Regression")
  
}

n = c(100, 1000, 10000)
p = c(10, 20, 50)

for (n.val in n) {
  for (p.val in p) {
    run_simulation(5, n.val, p.val, 0.05)
  }
}

