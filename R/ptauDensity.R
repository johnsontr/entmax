#' Density of the posterior distribution of model parameters when the intercept is fixed at its MAP value and the treatment effect is given
#'
#' @param tau.value a specific value for the treatment effect parameter
#' @param pt a \code{posteriorTheta()} object
#'
#' @return of the posterior distribution of model parameters when the intercept is fixed at its MAP value and the treatment effect is given
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' post <- posteriorTheta(df$Y, df$D, sigma0=1, lambda=1)
#' tau <- 0.1
#' posteriorTauDensity(tau, post)
ptauDensity <- function(tau.value, pt){
	###
	### This is the function that will need to be integrated.
	###

	# Fix the intercept at its MAP when calculating the density of a given value of tau
	theta.value <- as.vector(c(pt$mu[1], tau.value))
	return(mvtnorm::dmvnorm(theta.value, pt$mu, pt$Sigma))
}
