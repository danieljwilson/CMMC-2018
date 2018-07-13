source("1-objective.R")
#set up the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#sample parameters from priors for parameters of slot model
nSamples = '???'

#get a likelihood for all parameters sampled from the priors
slotLikelihoods = array(dim = c(nSamples))

#get marginal likelihood for slot model
