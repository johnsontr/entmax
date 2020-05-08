test_that("posteriorThetaDensity() works", {

  # SKIP FOR NOW
  # Make sure that the appropriate errors are thrown when someone uses 
  # the incorrect type of input for the function parameters.
  # SKIP FOR NOW
  
  df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)
  post <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)
  theta <- c(0.3,0.9)
  dens <- posteriorThetaDensity(theta, post)

  # Make sure the output is a scalar
  expect_equal(is.matrix(dens), FALSE)
  expect_equal(is.numeric(dens), TRUE)


})
