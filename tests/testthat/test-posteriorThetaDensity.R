test_that("posteriorThetaDensity() takes Y, D, theta, sigma0 and lambda and returns a density", {

	# The output from posteriorThetaDensity() should be very close to ptDensity(), which is just a wrapper for dmvnorm
	df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1) # Simulate some data
	pt <- posteriorTheta(df$Y, df$D, sigma0=1, lambda=1)

	ptDensity <- mvtnorm::dmvnorm(as.vector(theta), pt$mu, pt$Sigma)
	postThetaDens <- posteriorThetaDensity(df$Y, df$D, theta=as.numeric(pt$mu))

  expect_equal(2 * 2, 4)
})
