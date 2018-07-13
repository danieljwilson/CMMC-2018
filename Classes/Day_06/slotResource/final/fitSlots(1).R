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
