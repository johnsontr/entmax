#' Density of the posterior distribution of model parameters at a given value of those model parameters
#'
#' @param theta specific values for the model parameters; this needs to be of length \code{dim(as.matrix(cbind(1,D)))[2]}
#' @param ptmap a \code{posteriorThetaMAP()} object
#'
#' @return the density of the posterior distribution of the model parameter estimates at the specific theta values.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' postThetaMAP <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)
#' theta <- c(0.3,0.9)
#' posteriorThetaDensity(theta, postThetaMAP)
posteriorThetaDensity <- function(theta, ptmap){
  # The theoretical solution to the specified problem is distributed
  # multivariate normal with mean ptmap$mu and vcov matrix ptmap$Sigma, where
  # ptmap is a posteriorThetaMAP() object.

  return( mvtnorm::dmvnorm( as.vector(theta), ptmap$mu, ptmap$Sigma) )
}
