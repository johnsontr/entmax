test_that("posteriorThetaMAP() takes in data and returns Bayesian linear regression estimates for model parameters", {

  # SKIP FOR NOW
  # Make sure that the appropriate errors are thrown when someone uses 
  # the incorrect type of input for the function parameters.
  # SKIP FOR NOW

  df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)
  post <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)

  # Make sure the output is a list consisting of a 2x1 vector and a 2x2 vcov matrix
  # Make sure the output has the appropriate properties.

  # Make sure the output is a list
  expect_equal(typeof(post), "list")

  # Make sure the first element of the list can be used for matrix / vector algebra
  expect_equal(is.matrix(post$mu), TRUE)
  # Make sure the dimension of the first element of the list is 2x1
  expect_equal(dim(post$mu)[1], 2)
  expect_equal(dim(post$mu)[2], 1)
  # Make sure the elements of the first element of the list are numbers
  expect_equal(is.numeric(post$mu[1]), TRUE)
  expect_equal(is.numeric(post$mu[2]), TRUE)


  # Make sure the second element of the list can be used for matrix / vector algebra
  expect_equal(is.matrix(post$Sigma), TRUE)
  # Make sure the second element of the list is a 2x2 matrix
  expect_equal(dim(post$Sigma)[1], 2)
  expect_equal(dim(post$Sigma)[2], 2)
  # Make sure the elements of the second element of the list are numbers
  expect_equal(is.numeric(post$Sigma[1,1]), TRUE)
  expect_equal(is.numeric(post$Sigma[1,2]), TRUE)
  expect_equal(is.numeric(post$Sigma[2,1]), TRUE)
  expect_equal(is.numeric(post$Sigma[2,2]), TRUE)

})
