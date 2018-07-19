#Full diffusion model 
nreps <- 1000
nsamples <- 5000

drift <- 0.15 
s <- 0.1
a <- 0.1
sz <- 0.1
sv <- 0.18
st0 <- 180
ter <- 200
dt <- 0.001

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 
for (i in c(1:nreps)) { 
  sp <- runif(1,-sz*a, sz*a)
  v <- rnorm(1,drift,sv)
  evidence[i,] <- cumsum(c(sp,rnorm(nsamples,v*dt,sqrt(dt)*s)))  
  p <-  which(abs(evidence[i,])>a)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}
latencies <- latencies + runif(nreps,ter-st0/2,ter+st0/2)

layout(m=array(1:2,dim=c(1,2)))
hist(latencies[responses==1])
hist(latencies[responses==-1])
