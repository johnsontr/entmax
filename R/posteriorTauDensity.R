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
posteriorTauDensity <- function(Y, D, tau.value, sigma0=1, lambda=1){
	###
	### This is the function that will need to be integrated.
	###

	# Fix the intercept at its MAP when calculating the density of a given value of tau
	pt <- posteriorTheta(Y, D, sigma0=sigma0, lambda=lambda)
	theta <- as.matrix(c(pt$mu[1], tau.value))
 	X <- as.matrix(cbind(1,D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
 	Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors
  	
 	### Equation 13 from main.tex
  	return(as.numeric(exp((-1/(2)*sigma0^2) * crossprod(Y - (X %*% theta)) - (1/2)*(t(theta) %*% Lambda %*% theta))))
}
