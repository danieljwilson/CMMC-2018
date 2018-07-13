source("1-objective.R")
#the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#sample parameters from priors for parameters of slot model
nSamples = 1e4
priorK = runif(nSamples,0,8)
priorG = runif(nSamples,0,1)

#get a likelihood for all parameters sampled from the priors
slotLikelihoods = array(dim = c(nSamples))
for (i in 1:nSamples){
  slotLikelihoods[i] = slotObjective(x = c(priorK[i],priorG[i]), changeK = changeK, changeN = changeN, sameK = sameK, sameN = sameN, setSize = setSize)
}
#get marginal likelihood for slot model
slotMarginal = mean(slotLikelihoods)

#sample parameters from priors for parameters of the resource model
priorDPrime1 = runif(nSamples,0,4)
priorDPrime2 = runif(nSamples,0,4)
priorBeta = runif(nSamples,0.2,5)

#get a likelihood for all parameters sampled from the priors
resourceLikelihoods = array(dim = c(nSamples))
for (i in 1:nSamples){
  resourceLikelihoods[i] = resourceObjective(x = c(priorDPrime1[i],priorDPrime2[i],priorBeta[i]), changeK = changeK, changeN = changeN, sameK = sameK, sameN = sameN, setSize = setSize)
}
#get marginal likelihood for resource model
resourceMarginal = mean(resourceLikelihoods)

#calculate Bayes factor