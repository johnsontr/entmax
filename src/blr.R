# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 2, 2020

###
# Purpose: Functions that return various distributions associated with a given Bayesian linear regression.
#
# NOTE: For now, sigma0^2 is assumed to be known.
##

library(mvtnorm)

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
  
  X <- as.matrix(cbind(1,df$D)) # Add a column of 1s preceding the vector D for the intercept in the linear model.
  Lambda <- lambda*diag(dim(X)[2]) # Hyperpriors
  
  ###
  ### Equation 12 : posterior distribution of model parameter estimates
  # mu_theta = (X'X + (sigma0)^2 * Lambda)^{-1} * X'Y
  # Sigma_theta = ((sigma0)^2) * (X'X + (sigma0)^2 * Lambda)^{-1}
  ###
  
  # mu_theta = (X'X + (sigma0)^2 * Lambda)^{-1} * X'Y
  mu_theta <- solve(crossprod(X, X) + ((sigma0^2)*Lambda)) %*% t(X) %*% df$Y
  rownames(mu_theta) <- c("Intercept","ATE")
  colnames(mu_theta) <- c("Estimate")
  
  # Sigma_theta = ((sigma0)^2) * (X'X + (sigma0)^2 * Lambda)^{-1}
  Sigma_theta <- (sigma0^2)*solve(crossprod(X, X) + ((sigma0^2)*Lambda))
  rownames(Sigma_theta) <- c("Intercept","ATE")
  colnames(Sigma_theta) <- c("Intercept","ATE")
  
  return(list("mu" = mu_theta, "Sigma" = Sigma_theta))
}

posterior.predictive.outcome <- function(Y, D, d, sigma0=1, lambda=1){
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
  
  post.theta <- posterior.theta(Y, D, sigma0, lambda) # Get mu_theta and Sigma_theta to use in equation 11
  
  ###
  ### Equation 11
  # y | Y ~ MVN( x'mu_n , (sigma0)^2 + x'*Sigma_n*x )
  ###
  mu_y <- as.numeric( c(1,d) %*% as.vector(post.theta$mu) ) # Add a 1 preceding the new observation d for the intercept.
  Sigma_y <- as.numeric( sigma0^2 + ( t(c(1,d)) %*% as.matrix(post.theta$Sigma) %*% c(1,d) ) ) 
  
  return( list( "mu" = mu_y, "Sigma" = Sigma_y ) )
}

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
  return(dnorm(as.numeric(y), as.numeric(ppo[1]), as.numeric(ppo[2])))
}

pt.density <- function(theta, pt){
  ###
  ###
  # Function: return the density for a vector theta taking a certain value given the DGP
  #
  # Inputs:
  #     y: the value the new observation y takes
  #     ppo: a posterior.theta() object
  #
  # Output:
  #     density: the density of the posterior predictive distribution of the outcome for y=y
  ###
  ###
  return(dmvnorm(as.vector(theta), pt$mu, pt$Sigma))
}

posterior.theta.eval <- function(Y, D, theta, sigma0=1, lambda=1){
  ###
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



