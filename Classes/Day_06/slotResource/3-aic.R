source("1-objective.R")

#set up the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#get best-fit parameters for slot model
#set up best-fitting parameters for slot model
startK = 3.5
startG = 0.5

startPars = c(startK, startG)

#use optim to get best-fitting parameters
# lower is [0,8] for K and [0,1] for g

out_slots = optim(par = startPars,
                  fn = slotObjective,
                  changeK=changeK, changeN=changeN,
                  sameK=sameK, sameN=sameN,
                  setSize=setSize,
                  lower = c(0,0), upper = c(8,1),
                  method = c('L-BFGS-B'),
                  control = list(fnscale = -1))

#transform max-likelihood into AIC
slotAIC = -2 * log(out_slots$value) + 2 * length(startPars)


#do same for resource model
dPrime = c(3,1)
beta = 1          # this is unbiased point (see the objective function...because ln(1) = 0)

startPars = c(dPrime, beta)

out_resource = optim(par = startPars,
                     fn = resourceObjective,
                     changeK=changeK, changeN=changeN,
                     sameK=sameK, sameN=sameN,
                     setSize=setSize,
                     lower = c(-Inf,-Inf,0), upper = c(Inf,Inf,Inf),
                     method = c('L-BFGS-B'),
                     control = list(fnscale = -1))

resourceAIC = -2 * log(out_resource$value) + 2 * length(startPars)

