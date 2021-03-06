% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/posteriorTauDensity.R
\name{posteriorTauDensity}
\alias{posteriorTauDensity}
\title{Density of the posterior distribution of model parameters when the intercept is fixed at its MAP value and the treatment effect is given}
\usage{
posteriorTauDensity(tau, ptmap)
}
\arguments{
\item{tau}{a specific value for the treatment effect parameter}

\item{ptmap}{a \code{posteriorTheta()} object}
}
\value{
of the posterior distribution of model parameters when the intercept is fixed at its MAP value and the treatment effect is given
}
\description{
Density of the posterior distribution of model parameters when the intercept is fixed at its MAP value and the treatment effect is given
}
\examples{
df <- dgenRbinom()
postThetaMAP <- posteriorThetaMAP(df$Y, df$D, sigma0=1, lambda=1)
tau <- 0.1
posteriorTauDensity(tau, postThetaMAP)
}
