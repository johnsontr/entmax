# Starting point

###
# Binary Y, Binary D, no covariates
###

# Generate potential outcomes.

N <- 50

p <- 0.90   # Pr( Y = 1 | D = 1 ) = p
            # Pr( Y = 0 | D = 1 ) = 1-p

q <- 0.30   # Pr( Y = 1 | D = 0 ) = q
            # Pr( Y = 0 | D = 0 ) = 1-q

# Generate treatment vector
set.seed(02144)
D <- rbinom(N, 1, 0.5) # Assignment (1=treated, 0=control)
# Generate outcome vector
Y <- rep(NA, N)
set.seed(02144)
for (i in 1:N){
    if(D[i]==1){Y[i] <- rbinom(1,1,p)}
    else{Y[i] <- rbinom(1,1,q)}
}

# The new, (N+1)th experimental unit has arrived. 
# While I have coded the values p and q out of necessity, p and q would not be known to the researcher.
# Which would increase entropy of the estimate of the ATE the most?

# We don't know what the ATE 

# Model
# Y = a + b*D + u

# Use Bayesian linear regression 
# RSTAN



