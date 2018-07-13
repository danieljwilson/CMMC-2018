source("objective.R")
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
mean(slotLikelihoods)
