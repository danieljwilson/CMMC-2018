#diffusion model with dumb collapsing bounds 
nreps <- 1000
nsamples <- 5000

drift <- 0.15
s <- 0.1
start.a <- 0.1
end.a <- 0.03
time.to.collapse <- 350
a <- c(start.a,seq(start.a,end.a,length.out = time.to.collapse),rep(end.a,nsamples-time.to.collapse))
ter <- 200
dt <- 0.001

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 
for (i in c(1:nreps)) { 
  evidence[i,] <- cumsum(c(0,rnorm(nsamples,drift*dt,sqrt(dt)*s)))  
  p <-  which(abs(evidence[i,])>a)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}
latencies <- latencies + ter

layout(m=array(1:2,dim=c(1,2)))
hist(latencies[responses==1])
hist(latencies[responses==-1])

