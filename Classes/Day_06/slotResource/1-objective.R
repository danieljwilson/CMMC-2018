#likelihood for slot model
slotObjective = function(x, changeK, changeN, sameK, sameN, setSize){
  #set up parameters
  k = x[1]
  g = x[2]
  
  #set 'd', the probability of remembering the item
  d = pmin(k/setSize, 1)    # use pmin because we need the probability of remembering is greater than 1
    
  #get predicted probability for hits and false alarms
  predH = d + (1-d) * g     # pred. prob. of Hits
  predFA = (1-d) * g        # pred. prob. of False Alarms
  
  #evaluate likelihood
  likelihood = array(dim = c(2,length(setSize)))
  likelihood[1,] = dbinom(x = changeK, size = changeN, prob = predH)   # the second dimension [1,] is set size
  likelihood[2,] = dbinom(x = sameK, size = sameN, prob = predFA)

  #return the product of likelihoods
  return(prod(likelihood))
}

#likelihood for resource model
resourceObjective = function(x, changeK, changeN, sameK, sameN, setSize){
  #set up parameters
  dPrime = x[1:2]
  beta = x[3]
  
  #get predicted probability for hits and false alarms
  predH = pnorm(dPrime/2 - log(beta)/dPrime)
  predF = pnorm(-dPrime/2 - log(beta)/dPrime)
  
  #set up likelihood
  likelihood = array(dim = c(2,length(setSize)))
  likelihood[1,] = dbinom(x = changeK, size = changeN, prob = predH)
  likelihood[2,] = dbinom(x = sameK, size = sameN, prob = predF)
  
  return(prod(likelihood))
}
