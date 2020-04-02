# Author: T. Ryan Johnson
# Contact: thomas.johnson@wustl.edu
# Date: March 21, 2020

###
###
###
# MAIN
#
# This is the main script to execute and produce results using dgen, weights, and thompson.

# Call dgen to generate the data.

# Call weights to generate the weights to pass to the Thompson sampler.

# Assign treatment based on Thompson sampler.
# Find actual outcome for the (num.obs+1)th observation using dgen.
# Update data set. Start over for the (num.obs+2)th observation.