#' Posterior predictive distribution of the outcome
#'
#' Return the parameters of the posterior distribution of the coefficients of the linear model.
#' Data \code{(Y, D)} and potential input d must be provided; \code{sigma0} and \code{lambda} (the prior on \code{sigma0})
#' are assumed to be known.
#'
#' @param Y Outcome variable
#' @param D Treatment indicator variable
#' @param d New observation designation
#' @param sigma0 Variance common to all disturbances
#' @param lambda The prior on \code{sigma0}
#'
#' @return Return the predictive posterior distribution of the outcome for a given treatment designation and existing data.
#' @export
#'
#' @examples
#' df <- dgenRbinom()
#' posteriorPredictiveOutcome(df$Y, df$D,  d=0, sigma0=1, lambda=1)
#' posteriorPredictiveOutcome(df$Y, df$D,  d=1, sigma0=1, lambda=1)
posteriorPredictiveOutcome <- function(Y, D, d, sigma0=1, lambda=1){
  ###
  ###
  # Function: return the parameters of the posterior distribution of the coefficients of the linear model.
  #
  # Inputs:
  #     Y: an N-vector of outcomes
  #     D: an N-vector of treatment assignments
  #     d: The new data point.
  #     sigma0: Prior for the standard deviation of the disturbance term in the linear model
  #     lambda: Hyperprior for the standard deviation of the disturbance term in the linear model
  #
  # Outputs:
  #     mu_n: the mean vector of the posterior distribution of the vector of linear regression coefficients (intercept, coefficient on D, sigma0 known)
  #     Sigma_n: the variance-covariance matrix of the posterior distribution of the vector of linear regression coefficients (intercept, coefficient on D, sigma0 known)
  #     y.density: the value dnorm takes for mu_n and Sigma_n
  ###
  ###

  post.theta <- posteriorTheta(Y, D, sigma0, lambda) # Get mu_theta and Sigma_theta to use in equation 11

  ###
  ### Equation 11
  # y | Y ~ MVN( x'mu_n , (sigma0)^2 + x'*Sigma_n*x )
  ###
  mu_y <- as.numeric( c(1,d) %*% as.vector(post.theta$mu) ) # Add a 1 preceding the new observation d for the intercept.
  Sigma_y <- as.numeric( sigma0^2 + ( t(c(1,d)) %*% as.matrix(post.theta$Sigma) %*% c(1,d) ) )

  return( list( "mu" = mu_y, "Sigma" = Sigma_y ) )
}
