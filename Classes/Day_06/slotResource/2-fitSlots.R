# load in the function
source("1-objective.R")

#set up the data
changeK = c(35,25); changeN = c(40,40)
sameK = c(5,15); sameN = c(40,40)
setSize = c(4,8)

#set up best-fitting parameters for slot model
startK = 3.5
startG = 0.5

startPars = c(startK, startG)

#use optim to get best-fitting parameters
# lower is [0,8] for K and [0,1] for g

out = optim(pars = startPars,
            fn = slotObjective,
            changeK=changeK, changeN=changeN,
            sameK=sameK, sameN=sameN,
            setSize=setSize,
            lower = c(0,0), upper = c(8,1),
            method = c('L-BFGS-B'),
            control = list(fnscale = -1))

out
