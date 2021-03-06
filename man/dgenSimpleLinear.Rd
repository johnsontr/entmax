% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dgenSimpleLinear.R
\name{dgenSimpleLinear}
\alias{dgenSimpleLinear}
\title{Data generating process for a simple linear regression}
\usage{
dgenSimpleLinear(num.obs = 50, intercept = 2, treatment.effect = -4, seed = 1)
}
\arguments{
\item{num.obs}{the number of observations to generate in the data set; defaults to 50 observations}

\item{intercept}{the y-intercept of the linear model; defaults to 2}

\item{treatment.effect}{the coefficient on the treatment indicator  in the linear model; defaults to -4}

\item{seed}{randomization seed; defaults to 1.}
}
\value{
a \code{(num.obs x 2)} data frame where column 1 is the dependent variable \code{Y} and column 2 is the treatment indicator \code{D}
}
\description{
Data generating process for a simple linear regression
}
\examples{
dgenSimpleLinear(num.obs=50, intercept=2, treatment.effect=-4, seed=1)
}
