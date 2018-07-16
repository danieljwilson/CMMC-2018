#### Memory Measurement Model (MMM) for Cspan-Removal (data from Perth, 2013)

rm(list=ls())
graphics.off()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

library(R2jags)  
library(gplots)  # for barplot2 function 


################################## Data ###############################

Memdat <- read.table("CspanRFreq.dat")
names(Memdat) <- c("NcorrHigh", "NcorrLow", 
                   "NotherHigh", "NotherLow", 
                   "NDinposHigh", "NDinposLow",
                   "NDotherHigh", "NDotherLow", 
                   "NnplHigh", "NnplLow")         # Number of non presented

# We want to re-arrange the data into a 3-D matrix:
# rows = subjects
# columns = 2 conditions of free time (= cognitive load)
# pages = 5 response categories
dat <- array(0,dim=c(dim(Memdat)[1], 2, 5))
cond1 <- Memdat[,c(1,3,5,7,9)]
cond2 <- Memdat[,c(2,4,6,8,10)]
for (subj in 1:dim(Memdat)[1]) {
  for (cond in 1:2) {
    for (categ in 1:5) {
      if (cond == 1) dat[subj, cond, categ] <- cond1[subj,categ]
      if (cond == 2) dat[subj, cond, categ] <- cond2[subj,categ]
    }
  }
}

# now compute the total number of responses of each subject in each condition
N <- cbind(rowSums(dat[,1,1:5]), rowSums(dat[,2,1:5]))   
nsubj <- dim(dat)[1]
cellfreq <- c(1, 4, 1, 4, 5)          # how many candidates in candidate set go in each response category

#number of responses (out of 15) falling into each category 
# (1 correct, 4 other list items, 1 distractor in position, 
#4 other distractors, 5 NPL)

# put all the data Jags needs into a list
dataList = list(k = dat, 
                N = N,    # number of responses
                nsubj = nsubj,
                ch = cellfreq)

################################# RUN THE MCMC CHAINS  ##########################

modelname <- "CspanR_Template.txt"
traceS <- T     # flag to decide whether we want to monitor the parameters of individual subjects
parameters = c("muC" , "muA" , "muF" , "sgC" , "sgA", "kF")     # population level (could us muF & kF)
#parameters = c("muC" , "muA" , "aF" , "sgC" , "sgA", "bF")     # population level (could us muF & kF)
subjectparms <- c("C", "A", "F", "P")                          # subject level
if (traceS == T) parameters <- c(parameters, subjectparms)     # if we want to monitor subject parameters, we add them here


jags2Model <- jags.parallel(data=dataList, inits=NULL, 
                            parameters.to.save=parameters, 
                            model.file = modelname,
                            n.chains = 3, n.iter = 30000, 
                            n.burnin = 5000,
                            n.thin = 1, n.cluster= 3)

# convert rjags object to mcmc.list object, then convert into matrix
Samples <- as.mcmc(jags2Model)
mcmcchain = as.matrix( Samples )

muCchain <- mcmcchain[,"muC"]
muAchain <- mcmcchain[,"muA"]
muFchain <- mcmcchain[,"muF"]


#------------------------------------------------------------------------------
# EXAMINE THE RESULTS

# pull out the samples for the hyper-parameters (= group-level parameters) for means
hyperMeanSamples <- Samples[,c("muC" , "muA" , "muF"),]
plot(hyperMeanSamples) # convenient plot for mcmc.list objects
x11()
layout(matrix(1:4,2,2))
autocorr.plot(as.matrix(hyperMeanSamples), auto.layout=F, ask=F)
x11()
gelman.plot(hyperMeanSamples) # plots Gelman-Rubin R-hat

# pull out the samples for the hyper-parameters (= group-level parameters) for variability
hyperVarSamples <- Samples[,c("sgC" , "sgA", "kF"),]
plot(hyperVarSamples)
x11()
layout(matrix(1:4,2,2))
autocorr.plot(as.matrix(hyperVarSamples), auto.layout=F, ask=F)
x11()
gelman.plot(hyperVarSamples)

# a quick check of the parameter correlations (across samples) to check parameter trade-offs
x11()
crosscorr.plot(hyperMeanSamples)


#------------------------------------------------------------------------------
### Plot Posterior Densities of hyper-parameters for means

x11()
layout(matrix(1:4, 2, 2, byrow=T)) 

plot(density(muCchain), xlab="MuC", ylab="",  lwd=2, col="red", main="")
plot(density(muAchain), xlab="MuA", ylab="", lwd=2, col="red", main="")
plot(density(muFchain), xlab="MuF", ylab="", lwd=2, col="red", main="")


# scatterplot of pairwise combinations of group-mean parameters over samples
x11()
npar = 4  #number of parameters (from start of "parameters") for which correlations are plotted
npanels <- (npar-1)^2
x <- y <- (npar-1)
layout(matrix(1:npanels, x, y, byrow=T)) 
for (p1 in 1:(npar-1)) {
  for (p2 in (p1+1):npar) {
    parname1 <- parameters[p1]
    parname2 <- parameters[p2]
    v1 <- mcmcchain[,parname1]
    v2 <- mcmcchain[,parname2]
    plot(v1,v2, type="p", xlab=parname1, ylab=parname2)
  }
}


############### Plot Data and Predictions #######################################3

if (traceS == T) {
  
  nfreq <- 10
  predP <- matrix(0,nsubj,nfreq)
  predN <- matrix(0,nsubj,nfreq)
  datP <- matrix(0,nsubj,nfreq)
  for (idx in 1:nsubj) {
    for (cond in 1:2) {
      for (c in 1:5) {
        cdx <- (cond-1)*5+c  # condition index
        parameterName <- paste0("P[", idx, ",", cond, ",", c, "]") 
        Pchain <- mcmcchain[, parameterName] #pick the column required
        predP[idx,cdx] <- mean(Pchain)   
        predN[idx,cdx] <- predP[idx,cdx]*N[idx,cond]
        datP[idx,cdx] <- dat[idx,cond,c]/N[idx,cond]
      }
    }
  }
  
  
  x11(7,5)
  layout(matrix(1:6,2,3, byrow=T))
  par(cex=0.75)
  titletext = c("Correct", "Other Item", "Distractor in Position", "Other Distractor", "NPL")
  lgd = c("Data", "Model")
  
  for (cat in 1:5) {
    
    ylim <- c(0,0.25)
    if (cat == 1) ylim <- c(0,1)
    
    m <- matrix(0,2,2)
    std <- matrix(0,2,2)
    ci.u <- matrix(0,2,2)
    ci.l <- matrix(0,2,2)
    
    pdat <- datP[,c(cat, cat+5)]   # pick out the response category for conditions 1 and 2    
    # Bakeman-McArthur (1996) correction for simple computation of confidence intervals for within-subjects comparisons
    rmean = rowMeans(pdat)
    pcdat <- pdat - rmean + mean(rmean)
    #computation of CI
    for (c in 1:2) {
      m[1,c] <- mean(pcdat[,c])
      std[1,c] <- sd(pcdat[,c])
      ci.u[1,c] <- m[1,c] + 1.96*std[1,c]/sqrt(nsubj)    
      ci.l[1,c] <- m[1,c] - 1.96*std[1,c]/sqrt(nsubj)   
    }
    
    ppred <- predP[,c(cat, cat+5)]
    # Bakeman-McArthur correction
    rmean = rowMeans(ppred)
    pcpred <- ppred - rmean + mean(rmean)
    #computation of CI
    for (c in 1:2) {
      m[2,c] <- mean(pcpred[,c])
      std[2,c] <- sd(pcpred[,c])
      ci.u[2,c] <- m[2,c] + 1.96*std[2,c]/sqrt(nsubj)    
      ci.l[2,c] <- m[2,c] - 1.96*std[2,c]/sqrt(nsubj)   
    }
    
    barplot2(height=m, width=1, space = c(0.5, 0, 0.5, 0), legend.text=F,
             beside = TRUE, ylim=ylim,  xpd=F,
             density=c(0, 30), angle=45, col=c("white", "black"), prcol="white", border="black",
             names.arg = c("High CL", "Low CL"), 
             xlab="", ylab="Proportion Responses",
             plot.ci=TRUE, ci.l=ci.l, ci.u=ci.u, ci.color="black")  
    title(titletext[cat], cex.main=0.9)
    if (cat == 5) legend(0.5,0.2, legend=lgd, col=c("white", "black"), density=c(0,30))
  }
  
}





