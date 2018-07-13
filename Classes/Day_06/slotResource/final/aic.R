source("objective.R")
#the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#set up best-fitting parameters for slot model
startpars = c(3.5, 0.5)
out = optim(par = startpars, fn = slotObjective, changeN = changeN, sameN = sameN, changeK = changeK, sameK = sameK, setSize = setSize, lower = c(0,0), upper = c(8,1), method = c('L-BFGS-B'), control = list(fnscale = -1))

slotBestPars = out$par
slotMaxLikelihood = out$value
slotDeviance = - 2 * log(slotMaxLikelihood)
slotAIC = slotDeviance + 2 * length(slotBestPars)

#set up best-fitting parameters for resource model
startpars = c(2, 1, 1)
out = optim(par = startpars, fn = resourceObjective, changeN = changeN, sameN = sameN, changeK = changeK, sameK = sameK, setSize = setSize, lower = c(0,0,0), upper = c(5,5,20), method = c('L-BFGS-B'), control = list(fnscale = -1))

resourceBestPars = out$par
resourceMaxLikelihood = out$value
resourceDeviance = - 2 * log(resourceMaxLikelihood)
resourceAIC = resourceDeviance + 2 * length(resourceBestPars)
