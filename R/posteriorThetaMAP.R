#' Posterior distribution of model parameters
#'
#' Return the parameters of the posterior distribution of the coefficients of the linear model.
#' Data \code{(Y, D)} must be provided; \code{sigma0} and \code{lambda} (the prior on \code{sigma0}) are assumed to be known.
#'
#' @param Y an N-vector of outcomes
#' @param D an N-vector of treatment assignments
#' @param sigma0 prior for the standard deviation of the disturbance term in the linear model
#' @param lambda hyperprior for the standard deviation of the disturbance term in the linear model (\code{sigma0})
#'
#' @return return the mean vector and the variance covariance matrix for the posterior distribution of the model parameters; mu_n: the mean vector of the posterior distribution of the vector of linear regression coefficients; Sigma_n : the variance-covariance matrix of the posterior distribution of the vector of linear regression coefficients
#' @export
#'
#' @examples
#' df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1) # Simulate some data
#' posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)
posteriorThetaMAP <- function(Y, D, sigma0=1, lambda=1){

  X <- as.matrix(cbind(1,D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
  Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors

  ### Equation 12 from main.tex: posterior distribution of model parameter estimates
  # mu_theta = (X'X + (sigma0)^2 * Lambda)^{-1} * X'Y
  mu_theta <- solve(crossprod(X, X) + ((sigma0^2)*Lambda)) %*% t(X) %*% Y
  # Sigma_theta = ((sigma0)^2) * (X'X + (sigma0)^2 * Lambda)^{-1}
  Sigma_theta <- (sigma0^2)*solve(crossprod(X, X) + ((sigma0^2)*Lambda))

  # Formatting for output.
  rownames(mu_theta) <- c("Intercept","ATE")
  colnames(mu_theta) <- c("Estimate")
  rownames(Sigma_theta) <- c("Intercept","ATE")
  colnames(Sigma_theta) <- c("Intercept","ATE")

  # Return estimates for the model parameters and their standard errors as a list.
  return(list("mu" = mu_theta, "Sigma" = Sigma_theta))
}


