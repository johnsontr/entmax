# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 3, 2020

###
# Purpose: build the product of two multivariate normal pdfs and return it as a function.
###

#   # The posterior predictive distribution of the outcome assocaited new observation with D=d is equal to...
#   # INT p( ys | Y, D, sigma0sq = 1, xs=c(1,d), theta = (alpha, beta) ) * p( theta = (alpha, beta) |  Y, D, sigma0sq = 1, xs=c(1,d) ) d(theta)
#   #
#   # There is an analytical solution to the following.
#   # p( theta = (alpha, beta) |  Y, D, sigma0sq = 1, xs=c(1,d) )
#   # θ | y; X, σ^2_0 ∼ N( μ_n, Σ_n )
#   #
#   # p( ys | Y, D, sigma0sq = 1, xs=c(1,d), theta = (alpha, beta) )
#   
#   # Prob( y = 0 | d = 0, Y, D, sigma0sq )
#   
#   
#   # RWA - Gaussian Processes for Machine Learning (A.7) .200
#   # The product of two multivariate normal probability density functions
#   # is also a(n) (un-normalized) multivariate normal probability density function.
#   #
#   # The length of the mean vector is D.
#   #
#   # N(x|a, A) * N(x|b, B) = inv(Z) * N(x|c, C)
#   # where
#   # c = C*( inv(A)*a + inv(B)*b )
#   # and
#   # C = inv( inv(A) + inv(B) )
#   # and the normalizing constant is
#   # inv(Z) = ((2*pi)^(-D/2)) * (| A + B | ^(-1/2)) * exp( -(1/2) * (a-b)' inv(A+B) (a-b) )




