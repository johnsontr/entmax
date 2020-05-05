#' Posterior predictive distribution of the outcome
#'
#' Return the parameters of the posterior distribution of the coefficients of the linear model.
#' Data \code{(Y, D)} and potential input d must be provided; \code{sigma0} and \code{lambda} (the prior on \code{sigma0})
#' are assumed to be known.
#'
#' @param Y an N-vector of outcomes
#' @param D an N-vector of treatment assignments
#' @param d the treatment designation for the new observation
#' @param sigma0 prior for the standard deviation of the disturbance term in the linear model
#' @param lambda hyperprior for the standard deviation of the disturbance term in the linear model (\code{sigma0})
#'
#' @return return the mean and variance of the posterior predictive distribution of the dependent variable \code{Y}; mu_n: the mean vector of the posterior distribution of the model parameters; Sigma_n: the variance-covariance matrix of the posterior distribution of the model parameters
#' @export
#'
#' @examples
#' df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1) # Simulate some data
#' posteriorPredictive(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' posteriorPredictive(df$Y, df$D,  d=1, sigma0=1, lambda=1)
posteriorPredictive <- function(Y, D, d, sigma0=1, lambda=1){

  # Get the maximum a posteriori (MAP) estimate of the model parameters
  theta.MAP <- posteriorTheta(Y, D, sigma0, lambda)

  ### Equation 11 in main.tex: posterior predictive distribution of the outcome variable given the data and a 
  # y | Y ~ MVN( x'mu_n , (sigma0)^2 + x'*Sigma_n*x )
  mu_y <- as.numeric( c(1,d) %*% as.vector(theta.MAP$mu) ) # Add a 1 preceding the new observation d for the intercept.
  Sigma_y <- as.numeric( sigma0^2 + ( t(c(1,d)) %*% as.matrix(theta.MAP$Sigma) %*% c(1,d) ) ) # Sigma is *NOT* the standard deviation when Sigma isn't a matrix -- it is the variance.
  
  # Return the estimate for the predicted outcome and the estimate's standard error as a list object.
  return( list( "mu" = mu_y, "Sigma" = Sigma_y ) )
}


