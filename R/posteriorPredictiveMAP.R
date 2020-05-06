#' Maximum a posteriori estimate of the posterior predictive distribution of the outcome
#' NOTE: I need to verify the assumptions about the prior sigma0 and the hyperior lambda
#' necessary for the Bayesian MLE estimate to coincide with the MAP (i.e., the modal) estimate
#' of the posterior predictive distribution.
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
#' posteriorPredictiveMAP(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' posteriorPredictiveMAP(df$Y, df$D,  d=1, sigma0=1, lambda=1)
posteriorPredictiveMAP <- function( Y, D, d, sigma0=1, lambda=1 ){

  # Get the maximum a posteriori (MAP) estimate of the model parameters
  thetaMAP <- posteriorThetaMAP( Y, D, sigma0, lambda)
  newData <- c(1,d)

  ### Equation 11 in main.tex: posterior predictive distribution of the outcome variable given the data
  # y | Y ~ MVN( x'mu_n , (sigma0)^2 + x'*Sigma_n*x )
  mu_y <- as.numeric( newData %*% as.vector(thetaMAP$mu) ) # Add a 1 preceding the new observation d for the intercept.
  Sigma_y <- as.numeric( sigma0^2 + ( t(newData) %*% as.matrix(thetaMAP$Sigma) %*% newData ) ) # Sigma is *NOT* the standard deviation when Sigma isn't a matrix -- it is the variance.
  
  # Return the estimate for the predicted outcome and the estimate's standard error as a list object.
  return( list( "mu" = mu_y, "Sigma" = Sigma_y ) )
}


