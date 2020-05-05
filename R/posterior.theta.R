#' Posterior distribution of model parameters
#'
#' Return the parameters of the posterior distribution of the coefficients of the linear model.
#' Data (Y, D) must be provided; sigma0 and lambda (the prior on sigma0) are assumed to be known.
#'
#' @param Y Outcome variable
#' @param D Treatment indicator variable
#' @param sigma0 Variance common to all disturbances
#' @param lambda The prior on \code{sigma0}
#'
#' @return Parameter estimates for the coefficients of the linear model.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' posterior.theta(df$Y, df$D, sigma0=1, lambda=1)
posterior.theta <- function(Y, D, sigma0=1, lambda=1){
  ###
  ###
  # Function: return the parameters of the posterior distribution of the coefficients of the linear model.
  #
  # Inputs:
  #     Y: an N-vector of outcomes
  #     D: an N-vector of treatment assignments
  #     sigma0: Prior for the standard deviation of the disturbance term in the linear model
  #     lambda: Hyperprior for the standard deviation of the disturbance term in the linear model
  #
  # Outputs:
  #     mu_n: the mean vector of the posterior distribution of the vector of linear regression coefficients (intercept, coefficient on D, sigma0 known)
  #     Sigma_n : the variance-covariance matrix of the posterior distribution of the vector of linear regression coefficients (intercept, coefficient on D, sigma0 known)
  ###
  ###

  X <- as.matrix(cbind(1,D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
  Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors

  ###
  ### Equation 12 : posterior distribution of model parameter estimates
  # mu_theta = (X'X + (sigma0)^2 * Lambda)^{-1} * X'Y
  # Sigma_theta = ((sigma0)^2) * (X'X + (sigma0)^2 * Lambda)^{-1}
  ###

  # mu_theta = (X'X + (sigma0)^2 * Lambda)^{-1} * X'Y
  mu_theta <- solve(crossprod(X, X) + ((sigma0^2)*Lambda)) %*% t(X) %*% Y
  rownames(mu_theta) <- c("Intercept","ATE")
  colnames(mu_theta) <- c("Estimate")

  # Sigma_theta = ((sigma0)^2) * (X'X + (sigma0)^2 * Lambda)^{-1}
  Sigma_theta <- (sigma0^2)*solve(crossprod(X, X) + ((sigma0^2)*Lambda))
  rownames(Sigma_theta) <- c("Intercept","ATE")
  colnames(Sigma_theta) <- c("Intercept","ATE")

  return(list("mu" = mu_theta, "Sigma" = Sigma_theta))
}
