#diffusion model 
nreps <- 1000
nsamples <- 3000

s <- 0.1  #standard deviation of within-trial variability in accumulation
ter <- 300  #non-decision time
dt <- 0.001  #step size

a <- 0.1  #boundary separation

drift <- 0.1
# drift_var <- 0.01 (rnorm)
t0 = 0.           # starting point
t0_var =  # starting point var (runif)

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 

for (i in c(1:nreps)) { 
  # Trial updating
  t0 = t0 + t0_var
  drift = drift + drift_var
  
  # Evidence accumulation
  evidence[i,] <- cumsum(c(t0, rnorm(nsamples,drift*dt,sqrt(dt)*s)))  # nsamples1, nsamples2,
  
  p <-  which(abs(evidence[i,])>a)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}

# implement variable drift (one change)

latencies <- latencies + ter
print(length(latencies))

x11()
layout(m=array(1:2,dim=c(1,2)))
hist(latencies[responses==1])
hist(latencies[responses==-1])

