% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posteriorPredictiveMAP.R
\name{posteriorPredictiveMAP}
\alias{posteriorPredictiveMAP}
\title{Maximum a posteriori estimate of the posterior predictive distribution of the outcome
NOTE: I need to verify the assumptions about the prior sigma0 and the hyperior lambda
necessary for the Bayesian MLE estimate to coincide with the MAP (i.e., the modal) estimate
of the posterior predictive distribution.}
\usage{
posteriorPredictiveMAP(Y, D, d, sigma0 = 1, lambda = 1)
}
\arguments{
\item{Y}{an N-vector of outcomes}

\item{D}{an N-vector of treatment assignments}

\item{d}{the treatment designation for the new observation}

\item{sigma0}{prior for the standard deviation of the disturbance term in the linear model}

\item{lambda}{hyperprior for the standard deviation of the disturbance term in the linear model (\code{sigma0})}
}
\value{
return the mean and variance of the posterior predictive distribution of the dependent variable \code{Y}; mu_n: the mean vector of the posterior distribution of the model parameters; Sigma_n: the variance-covariance matrix of the posterior distribution of the model parameters
}
\description{
Return the parameters of the posterior distribution of the coefficients of the linear model.
Data \code{(Y, D)} and potential input d must be provided; \code{sigma0} and \code{lambda} (the prior on \code{sigma0})
are assumed to be known.
}
\examples{
df <- dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1) # Simulate some data
posteriorPredictiveMAP(df$Y, df$D,  d=0, sigma0=1, lambda=1)
posteriorPredictiveMAP(df$Y, df$D,  d=1, sigma0=1, lambda=1)
}
