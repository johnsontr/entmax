

# Take a data set. Use the default for dgenRbinom(), which has num.obs=50, p=0.3, q=0.9, and seed=1

df <- dgenRbinom()

# Find out what \hat{\theta} is since the quantity is used in equations (25) and (26)
post <- posteriorTheta(df$Y, df$D)
theta.hat <- post$mu # This is converted to a vector by my function calls.

# Right now, equations (25) and (26)

# Equation (22) is the posteriorThetaEval() function.
# Notice how Equation (22) requires a variable entry for the parameter estimates theta; this function needs to be integrable in \theta_{0}
# In Equation (22), \mathcal{D} is equal to the first two entries Y and D of posteriorThetaEval()
#                 \theta is equal to the


# Equation (25) and (26) are the logarithm of the posteriorTheta()


#ALGORITHM

