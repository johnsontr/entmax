test_that("posteriorPredictiveMAP() works", {

  # SKIP FOR NOW
  # Make sure that the appropriate errors are thrown when someone uses 
  # the incorrect type of input for the function parameters.
  # SKIP FOR NOW
  
  df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)
  d <- 0
  pred <- posteriorPredictiveMAP(df$Y, df$D, d=d, sigma0=1, lambda=1)

  # Make sure the output is a list
  expect_equal(typeof(pred), "list")

  # Make sure the first element of the list is a scalar
  expect_equal(is.matrix(pred$mu), FALSE)
  expect_equal(is.numeric(pred$mu), TRUE)

  # Make sure the second element of the list is a scalar
  expect_equal(is.matrix(pred$Sigma), FALSE)
  expect_equal(is.numeric(pred$Sigma), TRUE)


})
