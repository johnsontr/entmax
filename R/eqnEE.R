#' EquationEE
#'
#' @param Y Outcome variable
#' @param D Treatment indicator variable
#' @param theta Specific values for the model parameters
#' @param sigma0 Variance common to all disturbances
#' @param lambda The prior on \code{sigma0}
#'
#' @return The density of the posterior distribution of the model parameter estimates at the specified parameter values theta.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' thetaValues <- c(0.3,.9)
#' posterior.theta.eval(df$Y, df$D, thetaValues, sigma0=1, lambda=1)
posterior.theta.eval <- function(Y, D, theta, sigma0=1, lambda=1){
  ###
  ### NOTE: I think that this is eqnDD with fewer steps. Try to make posterior.predictive.outcome.eval()?
  ###
  # Function: return the density for a given vector theta of the posterior distribution of the coefficients of the linear model.
  #
  # Inputs:
  #     Y: an N-vector of outcomes
  #     D: an N-vector of treatment assignments
  #     theta: ; this needs to be of length dim(as.matrix(cbind(1,df$D)))[2]
  #     sigma0: Prior for the standard deviation of the disturbance term in the linear model
  #     lambda: Hyperprior for the standard deviation of the disturbance term in the linear model
  #
  # Output:
  #     density: the density of the posterior distribution
  ###
  ###

  theta <- as.matrix(theta)
  X <- as.matrix(cbind(1,df$D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
  Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors

  ###
  ### Equation 13
  return(as.numeric(exp((-1/(2)*sigma0^2) * crossprod(Y - (X %*% theta)) - (1/2)*(t(theta) %*% Lambda %*% theta))))
}
