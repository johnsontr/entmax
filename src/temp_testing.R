# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 3, 2020

###
### Testing
###
# df <- dgen.simpleLinear()
df <- dgen.rbinom()

posterior <- posterior.theta(df$Y, df$D)

posterior.c <- posterior.predictive.outcome(df$Y, df$D,  d=0, sigma0=1, lambda=1)
posterior.t <- posterior.predictive.outcome(df$Y, df$D,  d=1, sigma0=1, lambda=1)

ppo.density(y=1, posterior.c)
ppo.density(y=0, posterior.c)
ppo.density(y=1, posterior.t)
ppo.density(y=0, posterior.t)


