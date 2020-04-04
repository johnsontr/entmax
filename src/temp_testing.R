# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 3, 2020

# Make the data set. Binary Outcome Y, Binary Treament Indicator
df <- dgen.rbinom()

# Posterior distribution of the model parameters when Y = intercept + ATE*D + u
posterior.theta(df$Y, df$D)

# Posterior predictive distribution for the binary outcome when d=0
posterior.c <- posterior.predictive.outcome(df$Y, df$D,  d=0, sigma0=1, lambda=1)
# Posterior predictive distribution for the binary outcome when d=1
posterior.t <- posterior.predictive.outcome(df$Y, df$D,  d=1, sigma0=1, lambda=1)

# Posterior predictive distribution for the binary outcome when d=0
posterior.c 

# Posterior predictive distribution for the binary outcome when d=1
posterior.t 

# dnorm using the output from posterior.predictive.outcome when d=0 and y=0
ppo.density(y=0, posterior.c)

# dnorm using the output from posterior.predictive.outcome when d=0 and y=1
ppo.density(y=1, posterior.c)

# dnorm using the output from posterior.predictive.outcome when d=1 and y=0
ppo.density(y=0, posterior.t)

# dnorm using the output from posterior.predictive.outcome when d=1 and y=1
ppo.density(y=1, posterior.t)
