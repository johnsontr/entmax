#' Data generating process for a simple linear regression
#'
#' @param num.obs the number of observations to generate in the data set; defaults to 50 observations
#' @param intercept the y-intercept of the linear model; defaults to 2
#' @param treatment.effect the coefficient on the treatment indicator  in the linear model; defaults to -4
#' @param seed randomization seed; defaults to 1.
#'
#' @return a \code{(num.obs x 2)} data frame where column 1 is the dependent variable \code{Y} and column 2 is the treatment indicator \code{D}
#' @export
#'
#' @examples
#' dgenSimpleLinear(num.obs=50, intercept=2, treatment.effect=-4, seed=1)
dgenSimpleLinear <- function(num.obs=50, intercept=2, treatment.effect=-4, seed=1){
  #         NOTE: When num.obs = 50..........
  #         The seed=1 creates an unbalanced sample with 23 control observations and 27 treated observations.
  #         For a balanced sample when num.obs= 50, use seed=02134.

  # Set the seed.
  set.seed(seed)

  # Generate a vector of treatment assignment indicators.
  D <- stats::rbinom(num.obs, 1, 0.5) # Any exp. unit has a 50/50 chance of treatment.
  # Generate a vector of mean-zero i.i.d. noise.
  u <- stats::rnorm(50,0,1)
  # Generate the outcome vector.
  Y <- intercept + treatment.effect*D + u

  # Generate a data frame to return as output.
  return(data.frame(Y=Y,D=D))
}
