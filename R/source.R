# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: April 3, 2020

###
# Purpose: load custom made functions for the entropy maximizing online treatment assignment software.
###

# Set the path to the .R function files.
# PATH_TO_SOURCES <- "C:/Users/thomas.johnson/Documents/GitHub/entmax/R/" # Windows Desktop
PATH_TO_SOURCES <- "C:/Users/Thomas Johnson/Documents/GitHub/entmax/R/" # Windows MacBook

source(paste(PATH_TO_SOURCES, "dgen.R", sep="")) # Source files for data generating processes.
source(paste(PATH_TO_SOURCES, "blr.R", sep="")) # Source files for Bayesian linear regression.
#source(paste(PATH_TO_SOURCES, "builder.R", sep="")) # Source files for building the product of two multivariate normal pdfs.

print("Loaded the following functions: dgen.rbinom(), dgen.simpleLinear(), posterior.theta(), and posterior.predictive.outcome(), ppo.density(), and pt.density().")
