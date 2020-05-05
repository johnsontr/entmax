#' Density of the posterior distribution of model parameters at a given value of those model parameters.
#'
#' @param theta A vector of model coefficients
#' @param pt The posterior distribution of the model parameters.
#'
#' @return The density of the posterior distribution of the model parameter estimates at the specific theta values.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' post <- posterior.theta(df$Y, df$D, sigma0=1, lambda=1)
#' thetaValues <- c(0.3,0.9)
#' pt.density(thetaValues, post)
pt.density <- function(theta, pt){
  ###
  ###
  # Function: return the density for a vector theta taking a certain value given the DGP
  #
  # Inputs:
  #     theta: the value the new observation y takes
  #     pt: a posterior.theta() object
  #
  # Output:
  #     density: the density of the posterior distribution of the ATE for the outcome for model parameters theta
  ###
  ###
  return(mvtnorm::dmvnorm(as.vector(theta), pt$mu, pt$Sigma))
}
