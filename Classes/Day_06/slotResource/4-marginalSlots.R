source("slotResource/1-objective.R")
#set up the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#sample parameters from priors for parameters of slot model
nSamples = 10000

prior_k = runif(nSamples, min=0, max=10)
prior_g = runif(nSamples, min=0, max=1)

#params <- cbind(prior_k, prior_g)
params <- matrix(c(prior_k, prior_g), nrow = nSamples, ncol = 2)

#get a likelihood for all parameters sampled from the priors
slotLikelihoods = array(dim = c(nSamples))

for (i in 1:nSamples){
  slotLikelihoods[i] = slotObjective(params[i,],
                                     changeK=changeK, changeN=changeN,
                                     sameK=sameK, sameN=sameN,
                                     setSize=setSize)
}

#get marginal likelihood for slot model
mean(slotLikelihoods)
