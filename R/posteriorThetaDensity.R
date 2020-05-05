#' Density of the posterior distribution of the model parameter estimates at the specified values
#'
#' @param Y an N-vector of outcomes
#' @param D an N-vector of treatment assignments
#' @param theta specific values for the model parameters; this needs to be of length \code{dim(as.matrix(cbind(1,D)))[2]}
#' @param sigma0 prior for the standard deviation of the disturbance term in the linear model
#' @param lambda hyperprior for the standard deviation of the disturbance term in the linear model (\code{sigma0})
#'
#' @return the density of the posterior distribution of the model parameter estimates at the specified values theta
#' @export
#'
#' @examples
#' df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1) # Simulate some data
#' thetaValues <- c(0.3,.9) # Specify the model parameters (an intercept and a treatment effect)
#' posteriorThetaDensity(df$Y, df$D, thetaValues, sigma0=1, lambda=1)
posteriorThetaDensity <- function(Y, D, theta, sigma0=1, lambda=1){

  X <- as.matrix(cbind(1,D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
  Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors
  theta <- as.matrix(theta) # Format the theta vector

  ### Equation 13 from main.tex
  return(as.numeric(exp((-1/(2)*sigma0^2) * crossprod(Y - (X %*% theta)) - (1/2)*(t(theta) %*% Lambda %*% theta))))
}


