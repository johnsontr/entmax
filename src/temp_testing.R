# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 3, 2020



###
### Testing
###
df <- dgen.simpleLinear()
foo <- posterior.theta(df$Y, df$D)

foo1 <- posterior.predictive.outcome(df$Y, df$D,  d=1, sigma0=1, lambda=1)
foo2 <- posterior.theta(df$Y, df$D,  sigma0=1, lambda=1)

