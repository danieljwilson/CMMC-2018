## Casimir Ludwig, CMMC 2018, Perceptual Decision making

# This script demonstrates three methods for generating a predicted proportion correct for a 2-AFC design. This design is such that the subject sees both stimuli (e.g. signal and noise), and has to decide which interval or spatial location contained the signal. In our neural example, a mechanism might monitor two neural responses corresponding to the two decision alternatives, where those neural responses have been triggered by a single event (e.g. the random dot motion stimulus).

library(stats)
library(ggplot2)

rm(list=ls())

set.seed(NULL) # Initialise random number generator

mu_noise<-0 # noise distribution centred on 0
mu_signal<-1 # signal distribution centred on 1
sigma_noise<-1
sigma_signal<-1

x<-seq(mu_noise-4*sigma_noise,mu_signal+4*sigma_signal,by=0.1) # internal response scale
noise<-dnorm(x,mean=mu_noise,sd=sigma_noise)
signal<-dnorm(x,mean=mu_signal,sd=sigma_signal) 

# Method 1: Generate a predicted proportion correct from simulating many trials, i.e. many internal response pairs.
N<-10000
Rn<-rnorm(N,mu_noise,sigma_noise) # Draw random samples from noise distribution
Rs<-rnorm(N,mean=mu_signal,sd=sigma_signal) # Draw random samples from signal distribution
smax<-(Rs>Rn) # Logical vector with 1 for every pair in which signal > noise
# Deal with tied responses: flip a coin
tmp<-(Rs==Rn) # Logical vector with 1 for every tie

if(sum(tmp)>0){
  CoinFlips<-rbinom(N,1,0.5)
  smax[tmp]<-as.logical(CoinFlips[tmp])
}
PCsim<-sum(smax)/N;

# Method 2: Compute the predicted proportion correct by computing the
# difference between signal and noise distributions. The signal is
# correctly detected when signal > noise, i.e. when the difference is
# greater than 0.
mu_diff<-mu_signal-mu_noise # Mean of the difference distribution is the difference between the means
sigma_diff<-sqrt(sigma_signal^2+sigma_noise^2) # Variance of the difference distribution is the sum of the variances (sqrt to get SD)
# Compute the integral from 0 to inf of the difference distribution: note
# that this is simply 1 - the integral from -inf to 0. This is easy with
# Gaussian distributions.
PCdiff<-1-pnorm(0,mean=mu_diff,sd=sigma_diff)

# Method 3: Integrate across all possible noise responses and evaluate the
# probability that the signal response was greater. This results in a more
# complex expression to integrate, but the result should be identical to
# the difference distribution. The advantage of this approach is that it
# scales up for designs with more than two alternatives.
# First, set up the function to integrate
TwoAFCmax<-function(x,mean1=0,mean2=0,sd1=1,sd2=1){
  y=dnorm(x,mean=mean1,sd=sd1)*(1-pnorm(x,mean=mean2,sd=sd2))
  return(y)
}
# This function can only be numerically integrated. You can be more or less
# sophisticated about this. I'm using the built-in 'integrate'
# function. 
lowlim<-mu_noise-5*sigma_noise # Set some sensible integration limits
uplim<-mu_signal+5*sigma_signal # Assumes: mu_noise <= mu_signal
PC<-integrate(TwoAFCmax,
             lowlim,
             uplim,
             mean1=mu_noise,
             mean2=mu_signal,
             sd1=sigma_noise,
             sd2=sigma_signal)
PCint<-PC$value

# Plot signal and noise distributions
# cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# SDTdf=data.frame(rep(x,times=2),rep(c("noise","signal"),each=length(x)),c(noise,signal))
# names(SDTdf)=c("x","distribution","density")
# h1=ggplot(SDTdf,aes(x=x,y=density,colour=distribution))+
#   geom_line(size=2)+
#   xlab('internal response (normalised units)')+
#   ylab('probability density')+
#   scale_fill_manual(values=cbbPalette[1:2])+  # To use for fills
#   theme(panel.background = element_blank())+ # Get rid of the shading
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+ # Get rid of the grid lines
#   theme(axis.text.x  = element_text(size=12))+ # Change font size and angle on the x-axis
#   theme(axis.text.y = element_text(size=12))+ # Make font size the same on the y-axis
#   theme(axis.title.x=element_text(size=18))+ # Make font of the axes labels bigger
#   theme(axis.title.y=element_text(size=18))
# print(h1)
# 
# # Separately, show the resulting point on the psychometric function
# plot(mu_signal-mu_noise,PCdiff,xlim=c(0,2),ylim=c(0.5,1),xlab = "signal", 
# ylab = "PC")