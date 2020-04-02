# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: March 21, 2020

###
###
###
# Function: generate and return weights for use in a Thompson sampler for the
# (num.obs+1)th observation.
#
# The weights are a function of the expected Kuhlback-Leibler divergences.
#
# Assumes a data generating process for a continuous outcome variable Y
# linear in a binary treatment indicator D (no covariates).
#
# NOTE: When num.obs = 50..........
# The starting seed creates an unbalanced sample with 23 control and 27 treated.
# Use seed=02134 for a seed that has balanced treatment and control.
#
# GOAL: Use the same parameters as in dgen().

library(rstan) 
library(brms)
library(psych) #For some some extended summary statistics
library(tidyverse)
library(philentropy)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(rstanarm)
library(tibble)

###
### ORGANIZATION
###
### 1. To be completed.
### 2.
### 3. 
###

###
### SETUP

# Set the seed.
seed <- 1

# Generate the data.
df <- dgen()
colnames(df) <- c("Y","D")

# Visualize the data that was generated.
df %>% ggplot(aes(x = D,
             y = Y)) +
  geom_point(position = "jitter",
             alpha    = .05)+ #to add some random noise for plotting purposes
  theme_minimal()+
  geom_smooth(method = "lm",  # to add  the linear relationship
              aes(color = "Naive Difference in Means"),
              se = FALSE) +
  labs(title    = "Outcome vs. Treatment Assignment",
       x        = "Group Assignment: Control (D=0) or Treatment (D=1)",
       y        = "Outcome | D",
       color    = "Type of relationship" ) +
  theme(legend.position = "bottom")



###
### GENERATE MODELS WITH BRMS PACKAGE USING VARIOUS PRIORS

# Model 1 - Uninformative prior
model.1 <- brm(formula = Y ~ D, data=df, seed=seed)
# summary(model.1)
# posterior_summary(model.1)
# prior_summary(model.1)
# plot(model.1)

# Model 2 - Informative prior 1
priors2 <- c(set_prior("normal(0, 1)", class = "b", coef = "D"))
model.2 <- brm(formula = Y ~ D, data=df, prior=priors2, seed=seed)
# prior_summary(model.2)
# summary(model.2)
# posterior_summary(model.2)
# prior_summary(model.2)

# Model 3 - Informative prior 2
priors3 <- c(set_prior("normal(0, 2)", class = "b", coef = "D"))
model.3 <- brm(formula = Y ~ D, data=df, prior=priors3, seed=seed)
# prior_summary(model.3)
# summary(model.3)
# posterior_summary(model.3)
# prior_summary(model.3)

# Model 4 - Informative prior 3
priors4 <- c(set_prior("normal(2, 2)", class = "b", coef = "D"))
model.4 <- brm(formula = Y ~ D, data=df, prior=priors4, seed=seed)
# prior_summary(model.4)
# summary(model.4)
# posterior_summary(model.4)
# prior_summary(model.4)

# Model 5 - Informative prior 4
priors5 <- c(set_prior("normal(2, 1)", class = "b", coef = "D"))
model.5 <- brm(formula = Y ~ D, data=df, prior=priors5, seed=seed)
# prior_summary(model.5)
# summary(model.5)
# posterior_summary(model.5)
# prior_summary(model.5)



###
### SAMPLE FROM THE PRIOR AND THE POSTERIOR

# Sample from different priors for plotting.
prior1.2.3.4.5 <- bind_rows("Uninformative prior" = enframe(rnorm(10000, mean=0, sd=sqrt(1/1e-2))),
                            "Informative prior 1" = enframe(rnorm(10000, mean=0, sd=1)),
                            "Informative prior 2" = enframe(rnorm(10000, mean=0, sd=2)),
                            "Informative prior 3" = enframe(rnorm(10000, mean=2, sd=2)),
                            "Informative prior 4" = enframe(rnorm(10000, mean=2, sd=1)),
                            .id = "id1") %>% rename(b_D = value)

# Sample from different posteriors for plotting.
posterior1.2.3.4.5 <- bind_rows("Uninformative prior" = as_tibble(as.mcmc(model.1, pars = "b_D", exact_match = TRUE ,combine_chains = TRUE)),
                                "Informative prior 1" = as_tibble(as.mcmc(model.2, pars = "b_D", exact_match = TRUE ,combine_chains = TRUE)),
                                "Informative prior 2" = as_tibble(as.mcmc(model.3, pars = "b_D", exact_match = TRUE ,combine_chains = TRUE)),
                                "Informative prior 3" = as_tibble(as.mcmc(model.4, pars = "b_D", exact_match = TRUE ,combine_chains = TRUE)),
                                "Informative prior 4" = as_tibble(as.mcmc(model.5, pars = "b_D", exact_match = TRUE ,combine_chains = TRUE)),
                                .id = "id1")

# Bind the samples from the prior and posterior distribution.
priors.posterior <- bind_rows("Posterior" = posterior1.2.3.4.5, "Prior" = prior1.2.3.4.5, .id = "id2")

# NOTE: In the preceding...
# Instead of sampling the priors like this, I can get the actual prior values sampled by Stan by adding 
# the `sample_prior = TRUE` command to the `brm()` function. This would save the priors as used by stan.
# With enough samples this would yield the same results.
# Consider changing this for simplicity in subsequent versions.



###
### PLOT THE PRIOR AND POSTERIOR DISTRIBUTIONS

# Plot the different posteriors and priors.
ggplot(data    = priors.posterior, 
       mapping = aes(x        = b_D, 
                     fill     = id1, 
                     colour   = id2, 
                     linetype = id2,
                     alpha    = id2))+
    geom_density(size=1)+
    scale_x_continuous(limits=c(-10, 10))+
    scale_colour_manual(name   = 'Posterior/Prior', 
                        values = c("black","red"), 
                        labels = c("Posterior", "Prior"))+
    scale_linetype_manual(name   = 'Posterior/Prior',
                          values = c("solid","dotted"),
                          labels = c("Posterior", "Prior"))+
    scale_alpha_discrete(name   ='Posterior/Prior',
                         range  = c(.7,.3),
                         labels = c("Posterior", "Prior"))+
    scale_fill_manual(name   = "Densities",
                      values = c("Yellow","darkred","blue", "green", "pink"))+
    labs(title    = expression("Influence of (Informative) Priors on the Posterior Density of " ~ tau[D]), 
         subtitle = "Five posterior densities and their associated priors")



###
### FIND THE PREDICTIVE POSTERIOR DISTRIBUTION

# For the current sample (without the new observation yet), what is the estimate and the S.E. for the ATE?
# Computationally ...
# est.uninformative <- posterior_summary(model.1)[c("b_Intercept","b_D"), "Estimate"]
# est.uninformative.se <- posterior_summary(model.1)[c("b_Intercept","b_D"), "Est.Error"]
# est.informative   <- posterior_summary(model.5)[c("b_Intercept","b_D"), "Estimate"]
# est.informative.se <- posterior_summary(model.5)[c("b_Intercept","b_D"), "Est.Error"]
# Calculate the bias from the choice of prior.
# bias.prior <- round(100*((est.informative-est.uninformative)/est.uninformative), 2)
# But theoretically, the estimate \hat{D} from Bayesian linear regression is...

# Add the (num.obs + 1)th experimental unit.
newdata.control <- data.frame(D = c(0)) # The new experimental unit is in the control group.
newdata.treatment <- data.frame(D = c(1)) # The new experimental unit is in the control group.

# Draw from the posterior predictive distribution.
y.pred_uninformative_D0 <- posterior_predict(model.1, newdata=newdata.control, re_formula=NA)
y.pred_uninformative_D1 <- posterior_predict(model.1, newdata=newdata.treatment, re_formula=NA)
# dens_u0 <- hist(y.pred_uninformative_D0, freq=FALSE, breaks=100)
# dens_u1 <- hist(y.pred_uninformative_D1, freq=FALSE, breaks=100)
# 
# NOTE:
# These are draws from the predictive posterior distribution:
# what the outcome Y would be when the for the (num.obs + 1)th observation given the treatment assignment.
# As histograms, they are densities for the predicted outcome given whatever the treatment assignment is.
# I need to sample from the densities of:
#     P = p ( tau | E[ y_{num.obs + 1} | D_{num.obs + 1} = 0 ], previous_data )
# and 
#     Q = p ( tau | E[ y_{num.obs + 1} | D_{num.obs + 1} = 1 ], previous_data )
# How do I get to that from what I have above?
#
# CONSIDER:
# By definition, these samples have higher variance than samples of the means of the posterior predictive
# distribution computed by posterior_predict.brmsfit. This is because the residual error is incorporated
# in posterior_predict. However, the estimated means of both methods averaged across samples should be very
# similar.

# Kulback-Leibler Divergence between P and Q
P <- ecdf(y.pred_uninformative_D0)
plot(P)
Q <- ecdf(y.pred_uninformative_D1)
plot(Q)
x <- seq(-10,8,by=0.01)
P.prob <- P(x)
Q.prob <- Q(x)
vectors <- rbind(P.prob,Q.prob)
KL(vectors, unit="log")

# For later once I get the uninformative priors working.
# y.rep_informative1 <- posterior_predict(model.2, draws = 500)
# y.rep_informative2 <- posterior_predict(model.3, draws = 500)
# y.rep_informative3 <- posterior_predict(model.4, draws = 500)
# y.rep_informative4 <- posterior_predict(model.5, draws = 500)




# This will eventually be a weights function that is generalized. Right now, it's just a script.
# weights <- function(df, seed=1){
#   
# }




# Another KL divergence calculation option.
# library(philentropy)
# ?distance
# getDistMethods()
# Results depend on the dimension of x.
# nrow(x) = 2 ---> a single distance value
# nrow(x) > 2 ---> a matrix storing distance values for all pairwise probability vector comparisons.
# distance(x,
#         method="Kullback-Leibler",
#         test.na=TRUE)
# estimate.probability
# x <- runif(100)
# x.prob <- estimate.probability(x, method = "empirical")













