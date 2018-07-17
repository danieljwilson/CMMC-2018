## Observer model - skeleton code with a suggested structure

library(stats)
library(plyr)
library(Hmisc)

rm(list=ls())

setwd()

# PC_MAFC computes the predicted proportion correct for a single signal level. 
PC_MAFC<-function(mu_signal,mu_noise=0,sigma=1){
  # Define the function to be integrated
  MAFCmax<-function(x,mean1=0,mean2=0,sd1=1,sd2=1,M=2) dnorm(x,mean=mean1,sd=sd1)*(pnorm(x,mean=mean2,sd=sd2))^(M-1)
  
  Mstar<-4
  # This function can only be numerically integrated. You can be more or less
  # sophisticated about this. I'm using the built-in 'integrate'
  # function.
  lowlim<-mu_noise-5*sigma # Set some sensible integration limits
  uplim<-mu_signal+5*sigma # Assumes: mu_noise <= mu_signal
  PC<-integrate(MAFCmax,
               lowlim,
               uplim,
               mean1=mu_signal,
               mean2=mu_noise,
               sd1=sigma,
               sd2=sigma,
               M=Mstar)
  PC_MAFCval<-PC$value
  return(PC_MAFCval)
}

# Write the error function that returns the deviance for a single subject 
MyModel<-function(parms,PFdata){
  return(MyDeviance)
}

# Write a function that finds the best-fitting parameter(s) for a single subject: in other words: find the MLE parameters for MyModel, using optim or optimise. Having a function that does this will make it easier to efficiently fit multiple subjects in one go (rather than a for-loop)---have a look at the apply family of functions.
# However, you don't have to write a specific function and you could simply embed an 'optim(ize)' call in a for-loop that cycles through each participant separately.

# Load the data file and fit the model (see previous comment).

# Plot the results: observed data and model predictions superimposed. Do this separately for each subject. Better still: create a single figure with 8 subplots.