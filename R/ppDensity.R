#' Density of the posterior predictive distribution at the given outcome value y
#'
#' Return the density for y taking a certain value given the DGP. Requires
#' output from a \code{posteriorPredictiveOutcome()} object and the outcome value \code{y} at which
#' the density is measured.
#'
#' @param y the value taken by the new output
#' @param pp A \code{posteriorPredictive()} object
#'
#' @return the density of the posterior predictive distribution of the outcome taking value \code{y}
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' postPred <- posteriorPredictive(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' ppDensity(y=1, postPred)
ppDensity <- function(y, pp){
  return(stats::dnorm(as.numeric(y), as.numeric(pp[1]), as.numeric(pp[2])))
}


