#' Density of the posterior distribution of model parameters at a given value of those model parameters.
#'
#' @param theta specific values for the model parameters; this needs to be of length \code{dim(as.matrix(cbind(1,D)))[2]}
#' @param pt a \code{posteriorTheta()} object
#'
#' @return the density of the posterior distribution of the model parameter estimates at the specific theta values.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' post <- posteriorTheta(df$Y, df$D, sigma0=1, lambda=1)
#' thetaValues <- c(0.3,0.9)
#' ptDensity(thetaValues, post)
ptDensity <- function(theta, pt){
  # The theoretical solution to the specified problem is distributed
  # multivariate normal with mean pt$mu and vcov matrix Sigma, where
  # pt is a posteriorTheta() object (w9hich is a list consisting of mu and Sigma)
  return(mvtnorm::dmvnorm(as.vector(theta), pt$mu, pt$Sigma))
}
