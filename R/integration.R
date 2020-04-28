# Experiment with integrate, integrate2, integrate3



integrate(dnorm, -1.96, 1.96)
integrate(dnorm, -Inf, Inf)


     
## some functions do not handle vector input properly
f <- function(x) 2.0
try(integrate(f, 0, 1))
integrate(Vectorize(f), 0, 1)  ## correct
integrate(function(x) rep(2.0, length(x)), 0, 1)  ## correct
     


