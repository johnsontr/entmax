#' Data generating process for a simple linear regression
#'
#' @param num.obs The number of observations.
#' @param intercept The y-intercept of the linear model.
#' @param treatment.effect The coefficient on the treatment indicator.
#' @param seed The seed for randomization.
#'
#' @return A data frame of Y and D with \code{num.obs} total observations.
#' @export
#'
#' @examples
#' dgenSimpleLinear(num.obs=50, intercept=2, treatment.effect=-4, seed=1)
dgenSimpleLinear <- function(num.obs=50, intercept=2, treatment.effect=-4, seed=1){
  ###
  ###
  # Function: a data generating process for a simple linear data generating process.
  #
  # Input:
  #     num.obs: the number of observations to generate in the data set; defaults to 50 observations.
  #     intercept: the intercept of the linear model; defaults to 2
  #     treatment.effect: the coefficient for the treatment assignment indicator in the linear model; defaults to -4
  #     seed: randomization seed; defaults to 1.
  #
  #         NOTE: When num.obs = 50..........
  #         The seed=1 creates an unbalanced sample with 23 control observations and 27 treated observations.
  #         For a balanced sample when num.obs= 50, use seed=02134.
  #
  #         NOTE: Many data sets can be generated using apply and a vector of unique seeds.
  #
  # Output:
  #     df: a (num.obs x 2) data frame where column 1 is Y and column 2 is D.
  ###
  ###

  # Set the seed.

  set.seed(seed)

  # Generate a vector of treatment assignment indicators.
  D <- rbinom(num.obs, 1, 0.5) # Any exp. unit has a 50/50 chance of treatment.
  # Generate a vector of mean-zero i.i.d. noise.
  u <- rnorm(50,0,1)
  # Generate the outcome vector.
  Y <- intercept + treatment.effect*D + u

  # Generate a data frame to return as output.
  return(data.frame(Y=Y,D=D))
}
