#' Density of posterior predictive distribution at the given outcome value y
#'
#' Return the density for y taking a certain value given the DGP. Requires
#' output from posterior.predictive.outcome and the outcome value at which
#' the density is measured.
#'
#' @param y Outcome variable value
#' @param ppo A posterior.predictive.outcome() object
#'
#' @return The density of the posterior predictive distribution of the outcome taking value y
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' ppoD0 <- posterior.predictive.outcome(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' ppo.density(y=1, ppoD0)
ppo.density <- function(y, ppo){
  ###
  ###
  # Function: return the density for y taking a certain value given the DGP.
  #
  # Inputs:
  #     y: the value the new observation y takes
  #     ppo: a posterior.predictive.outcome() object
  #
  # Output:
  #     density: the density of the posterior predictive distribution of the outcome for y=y
  ###
  ###
  return(stats::dnorm(as.numeric(y), as.numeric(ppo[1]), as.numeric(ppo[2])))
}
