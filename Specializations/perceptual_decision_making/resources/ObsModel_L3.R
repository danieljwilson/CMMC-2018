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
  parm1<-parms[1]
  sdnoise<-1
  signal= # pass contrast levels through the transducer
  # For each signal level, compute the predicted PC (hint: check the apply family)
  # Once you have your predictions, compute the log-likelihoods and the Deviance
  return(MyDeviance)
}

# Load the data file and turn Participant into a factor

# One method is to use a for-loop. On each iteration, you select data from one participant and then call optim(ize) with 'MyModel' as the function to be minimised.

# Another method is to create a separate function that takes in a dataframe and calls optim(ize). You could then use this function in an apply-like statement to loop through the participants. If you have do not have a firm grasp on what these apply functions do, I suggest you stick with a for-loop.

# Try to do the fitting and end up with all the model parameters in a 8 x K data frame.

# Plot the results: observed data and model predictions superimposed. Do this separately for each subject, so you can see the quality of your fits.