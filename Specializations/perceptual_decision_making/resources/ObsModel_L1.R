## Observer model - skeleton code with a suggested structure

library(stats)
library(plyr)
library(Hmisc)

rm(list=ls())
setwd()

# Write a function that computes the predicted proportion correct for a single signal level. 

# Write the error function that returns the deviance for a single subject 


# Write a function that calls optimise (or optim, depending on the number of parameters) to minimise the error function you have written.

# Load the data file

# With the observed data, fit the model to each subject. You could use a for loop, which is fine. Alternatively, you can look at the apply family of functions.

# Plot the results: observed data and model predictions superimposed. Do this separately for each subject. Better still: create a single figure with 8 subplots.
