test_that("posteriorPredictiveDensity() works", {

  # SKIP FOR NOW
  # Make sure that the appropriate errors are thrown when someone uses 
  # the incorrect type of input for the function parameters.
  # SKIP FOR NOW
  
  df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)
  d <- 0
  pred <- posteriorPredictiveMAP(df$Y, df$D, d=d, sigma0=1, lambda=1)
  y <- 1
  dens <- posteriorPredictiveDensity(y=y, pred)

  # Make sure the output is a scalar
  expect_equal(is.matrix(dens), FALSE)
  expect_equal(is.numeric(dens), TRUE)

})
