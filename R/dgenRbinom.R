#' Data generating process for biased binary outcome under treatment.
#'
#' @param num.obs The number of observations.
#' @param p The probability of the outcome being 1 when treated.
#' @param q The probability of the outcome being 1 when treated.
#' @param seed The seed for randomization.
#'
#' @return A data frame of Y and D with \code{num.obs} total observations.
#' @export
#'
#' @examples
#' dgenRbinom(num.obs=50, p=0.3, q=0.9, seed=1)
dgenRbinom <- function(num.obs=50, p=0.3, q=0.9, seed=1){
  ###
  ###
  # Function: a data generating process for a
  #   i) zero intercept,
  #   ii) binary outcome, and
  #   iii) no covariate
  # data generating process using rbinom().
  #
  # Input:
  #     num.obs: the number of observations to generate in the data set; defaults to 50 observations.
  #     p: Pr( Y = 1 | D = 0 ) ; defaults to 0.3
  #     q: Pr( Y = 1 | D = 1 ) ; defaults to 0.9
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

  # Generate the num.obs x 1 vector of treatment assignment indicators.
  D <- stats::rbinom(num.obs, 1, 0.5) # Any exp. unit has a 50/50 chance of treatment.

  # Generate outcomes associated with the treatment assignment indicators D.
  Y <- rep(NA, num.obs)
  for(i in 1:num.obs){
    if(D[i]==0){ # If the exp. unit was part of the control group, ...
      Y[i] <- stats::rbinom(1,1,p) # then Y = 1 with probability p.
    } else { # If the exp. unit was part of the treatment group, ...
      Y[i] <- stats::rbinom(1,1,q) # then Y = 1 with probability q.
    }
  }

  # Generate a data frame to return as output.
  return(data.frame(Y=Y, D=D))
}
