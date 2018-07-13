source("objective.R")

setSize = c(4,8)

#sample parameters from priors for parameters of slot model
nSamples = 1e5
priorK = runif(nSamples,0,1)
priorG = runif(nSamples,0,1)

preds = array(dim = c(2,length(setSize),nSamples))
for (i in 1:nSamples){
  tmp = slotPreds(c(priorK[i],priorG[i]), setSize = setSize)
  preds[1,,i] = tmp$pH; preds[2,,i] = tmp$pF
}

layout(m = array(1:2,dim = c(1,2))); par(mar = c(4,4,1,1))
joint.density.plot(y = preds[1,1,], x = preds[2,1,], col = F)
joint.density.plot(y = preds[1,2,], x = preds[2,2,], col = F)


priorK = rnorm(nSamples,3.5/8,0.33/8)
priorG = rnorm(nSamples,0.5,0.15)

preds = array(dim = c(2,length(setSize),nSamples))
for (i in 1:nSamples){
  tmp = slotPreds(c(priorK[i],priorG[i]), setSize = setSize)
  preds[1,,i] = tmp$pH; preds[2,,i] = tmp$pF
}

layout(m = array(1:2,dim = c(1,2))); par(mar = c(4,4,1,1))
joint.density.plot(y = preds[1,1,], x = preds[2,1,], col = F)
joint.density.plot(y = preds[1,2,], x = preds[2,2,], col = F)
