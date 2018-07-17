## Cas Ludwig, CMMC July 2018
## Perceptual decisions tournament. This script will fit the most basic model that just contains a linear transducer and assumes additive internal noise (with a value set to 1). This script demonstrates fitting the model in a (single level) Bayesian way.

library(stats)
library(plyr)
library(Hmisc)
library(R2jags)

rm(list=ls())

setwd("")

PC_MAFC=function(mu_signal,mu_noise=0,sigma=1){
  # Define the function to be integrated
  MAFCmax=function(x,mean1=0,mean2=0,sd1=1,sd2=1,M=2) dnorm(x,mean=mean1,sd=sd1)*(pnorm(x,mean=mean2,sd=sd2))^(M-1)
  
  Mstar=4
  # This function can only be numerically integrated. You can be more or less
  # sophisticated about this. I'm using the built-in 'integrate'
  # function.
  lowlim=mu_noise-5*sigma # Set some sensible integration limits
  uplim=mu_signal+5*sigma # Assumes: mu_noise <= mu_signal
  PC=integrate(MAFCmax,
               lowlim,
               uplim,
               mean1=mu_signal,
               mean2=mu_noise,
               sd1=sigma,
               sd2=sigma,
               M=Mstar)
  PC_MAFCval=PC$value
  return(PC_MAFCval)
}

# Load the data file
PF=read.csv("TrainData.csv",header=TRUE)
PF$Participant=as.factor(PF$Participant)
# I'm just going to add some information---in particular, binomial standard errors (to be used for plotting)
PF$PC=PF$Ncorrect/PF$Ntrials # proportion correct
PF$BSE=sqrt((PF$PC*(rep(1,times=length(PF$PC))-PF$PC))/PF$Ncorrect) # Binomial standard error

# Set up the data
contrastMatrix <- matrix(data=NA,nrow=length(unique(PF$Participant)),ncol=15)
nCorrect <- contrastMatrix
nTotal <- contrastMatrix
nPerCondition <- vector(mode="integer",length=length(unique(PF$Participant)))

for (i in 1:length(unique(PF$Participant))){
  tmpPF<-subset.data.frame(PF,Participant==unique(PF$Participant)[i])
  nPerCondition[i] <- dim(tmpPF)[1]
  contrastMatrix[i,1:nPerCondition[i]] <- t(tmpPF$rootEnergy)
  nCorrect[i,1:nPerCondition[i]] <- t(tmpPF$Ncorrect)
  nTotal[i,1:nPerCondition[i]] <- t(tmpPF$Ntrials)
}

jList=list("rootEnergy"=contrastMatrix,
           "n_per_condition"=nPerCondition,
           "nCorrect"=nCorrect,
           "nTrials"=nTotal,
           "NsampIntegral"=1000)
# Set random initial values for the chains
nChains=4
monparms=c("beta","PCpred") # Which parameters to monitor?
lam_mcmc <- jags(jList, parameters.to.save=monparms,
                 model.file ="lamJAGS_SLB_stripped.j", n.chains=nChains, n.iter=5000,
                 n.burnin=2000, n.thin=1, DIC=T) # When running multiple chains, consider using jags.parallel with n.cluster = nChains
mcmcSamples <- as.mcmc(lam_mcmc)
mcmcChains <- as.data.frame(as.matrix(mcmcSamples))

# Uncomment the following lines if you wanted to see some diagnostic information about the posterior and the MCMC chain.
#plot(lam_mcmc)
#acfplot(lam_mcmc)
#gelman.plot(lam_mcmc)

# Set up an empty dataframe that is going to contain some information about the posterior parameterestimate
parmDF=data.frame("Participant"=unique(PF$Participant),"postMean"=rep(0,times=8),"postMedian"=rep(0,times=8),"post2.5"=rep(0,times=8),"post97.5"=rep(0,times=8))

# We'll plot the psychometric functions in the loop below in which we're estimating parameters for each participant separately
xc=seq(0,4,length.out=100) #Vector of contrast values for plotting a smooth function
x11()
par(font.axis=1,font.lab=2)
par(mfcol=c(2,4))

# Now write some summary measures of the posterior distribution to a dataframe and plot the estimated psychometric functions
for (i in 1:length(unique(PF$Participant))){
  tmpPF<-subset.data.frame(PF,Participant==unique(PF$Participant)[i])
  parmDF$postMean[i]=mean(mcmcChains[,i])
  parmDF$postMedian[i]=median(mcmcChains[,i])
  parmDF[i,4:5]=quantile(mcmcChains[,i],c(0.025,0.975))
  # plot psychometric function with the mean of the posterior distribution of beta
  plot(tmpPF$rootEnergy,tmpPF$PC,type="p",sub=paste("S",i),xlab="contrast",ylab="PC",ylim=c(0.2,1))
  errbar(tmpPF$rootEnergy,tmpPF$PC,tmpPF$PC-tmpPF$BSE,tmpPF$PC+tmpPF$BSE,add=TRUE)
  ypred=sapply(xc*parmDF$postMean[i],PC_MAFC,mu_noise=0,sigma=1)
  lines(xc,ypred,col="grey",lty=1,lwd=2)
}