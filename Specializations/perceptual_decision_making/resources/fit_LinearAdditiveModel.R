## Cas Ludwig, CMMC July 2018
## Perceptual decisions tournament. This script will fit the most basic model that just contains a linear transducer and assumes additive internal noise (with a value set to 1). 

library(stats)
library(plyr)
library(Hmisc)

rm(list=ls())

setwd("C:/Users/Casimir Ludwig/Google Drive/CMSS2018/Cas/perceptualDecisions/Resources")

# First, the functions we'll need.

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

# LinearAdditiveModel is the error function that we are going to minimise for a single subject at a time. 
LinearAdditiveModel<-function(parms,PFdata){
 betaparm<-parms[1]
 sdnoise<-1
 LL<-0 # initialise log-likelihood
 signal<-betaparm*PFdata$rootEnergy
 # Use sapply to compute PC for each contrast level
 predPC<-sapply(signal,PC_MAFC,mu_noise=0,sigma=sdnoise)
 LL<-dbinom(PFdata$Ncorrect,PFdata$Ntrials,predPC)
 Min2LL<- -2*sum(log(LL))
 return(Min2LL)
}

# FindBestModel is a function that... finds the best fitting model for a single participant by minimising the error function above. This is the function we are going to call for each participant.
FindBestModel<-function(PFDF){
  startparms<-1 # pick some reasonable number for your starting parameters
# Optimisation using 'optim'
#  LinAddModel<-optim(startparms,
#                    LinearAdditiveModel,
#                    PFdata=PFDF)
#  return(c(LinAddModel$par,LinAddModel$value))  
 
# For the one-parameter model, we may as well use 'optimize' 
  LinAddModel<-optimize(LinearAdditiveModel,
                     interval=c(0,10),
                     PFdata=PFDF)
  return(c(LinAddModel$minimum,LinAddModel$objective))  
  #  return(data.frame(Participant=PFDF$Participant[1],BetaParm=LinAddModel$par,Minus2LL=LinAddModel$value))
}

# Now we have all the functions we need, we can actually start fitting our data.

# Load the data file
PF<-read.csv("TrainData.csv",header=TRUE)
PF$Participant<-as.factor(PF$Participant)
# I'm just going to add some information---in particular, binomial standard errors (to be used for plotting)
PF$PC<-PF$Ncorrect/PF$Ntrials # proportion correct
PF$BSE<-sqrt((PF$PC*(rep(1,times=length(PF$PC))-PF$PC))/PF$Ncorrect) # Binomial standard error

# Set up a dataframe to contain the model fitting results: for each participant, we have the best fitting parameter, and the deviance.
ParmDF<-ddply(.data=PF,.variables="Participant",.fun=FindBestModel)
names(ParmDF)<-c("Participant","BetaParm","Minus2LL") # Give some sensible names to the variables in the dataframe

# Plot the results. I know I'm using a for loop here, but figuring out how to do it in any other way is simply not worth the bother for 8 subjects.
xc<-seq(0,3.5,length.out=100) #Vector of contrast values for plotting a smooth function
x11()
par(font.axis=1,font.lab=2)
par(mfcol=c(2,4))
for (i in 1:nrow(ParmDF)){
  #par(mfg=c(1,1))
  tmpDF<-PF[PF$Participant==ParmDF$Participant[i],]
  plot(tmpDF$rootEnergy,tmpDF$PC,type="p",sub=paste("S",i),xlab="contrast",ylab="PC",ylim=c(0.2,1))
  errbar(tmpDF$rootEnergy,tmpDF$PC,tmpDF$PC-tmpDF$BSE,tmpDF$PC+tmpDF$BSE,add=TRUE)
  ypred<-sapply(xc*ParmDF$BetaParm[i],PC_MAFC,mu_noise=0,sigma=1)
  lines(xc,ypred,col="grey",lty=1,lwd=2)
}