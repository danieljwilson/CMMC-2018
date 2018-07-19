#diffusion model 
nreps <- 1000
nsamples <- 5000

drift <- 0.15
s <- 0.1
a <- 0.1
ter <- 200
dt <- 0.001
switch <- 500
new.drift <- -0.15

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 

for (i in c(1:nreps)) { 
  evidence[i,1:(switch+1)] <- cumsum(c(0,rnorm(switch,drift*dt,sqrt(dt)*s)))
  evidence[i,(switch+1):nsamples] <- cumsum(c(evidence[switch+1],rnorm(nsamples-switch-1,new.drift*dt,sqrt(dt)*s)))
  p <-  which(abs(evidence[i,])>a)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}
latencies <- latencies + ter

layout(m=array(1:2,dim=c(1,2)))
hist(latencies[responses==1])
hist(latencies[responses==-1])

