#' Density of the posterior predictive distribution at the given outcome value y
#'
#' Return the density for y taking a certain value given the DGP. Requires
#' output from a \code{posteriorPredictiveMAP()} object and the outcome value \code{y} at which
#' the density is measured.
#'
#' @param y the value taken by the new output
#' @param ppmap A \code{posteriorPredictiveMAP()} object
#'
#' @return the density of the posterior predictive distribution of the outcome taking value \code{y}
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' postPredMAP <- posteriorPredictiveMAP(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' posteriorPredictiveDensity(y=1, postPredMAP)
posteriorPredictiveDensity <- function( y, ppmap ){
  # The theoretical solution to the specified problem is distributed
  # multivariate normal with mean ppmap$mu and variance ppmap$Sigma, where
  # ppmap is a posteriorPredictiveMAP() object.
  return( stats::dnorm( y, ppmap[1], ppmap[2] ) )
}


