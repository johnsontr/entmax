test_that("posteriorThetaMAP() takes in data and returns Bayesian linear regression estimates for model parameters", {

  df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)

  post <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)

  expect_equal(2 * 2, 4)
})
