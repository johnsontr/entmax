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
#' postThetaMAP <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)
#' tau <- 0.1
#' posteriorTauDensity(tau, postThetaMAP)
posteriorTauDensity <- function( tau, ptmap ){
	###
	### This is the function that will need to be integrated.
	###

	# The theoretical solution to the specified problem is distributed
 	# multivariate normal with mean ptmap$mu and vcov matrix ptmap$Sigma, where
 	# ptmap is a posteriorThetaMAP() object -- a list consisting of mu and Sigma.

	# Fix the intercept at its MAP when calculating the density for the given value of tau.
	theta <- as.vector( c( ptmap$mu[1], tau) )
	return( mvtnorm::dmvnorm( theta, ptmap$mu, ptmap$Sigma) )
}
