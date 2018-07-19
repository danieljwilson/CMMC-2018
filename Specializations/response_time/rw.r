#random walk model 
nreps <- 1000
nsamples <- 2000

drift <- 0.0  #noninformative stimulus 
sdrw <- 0.3
criterion <- 3 

latencies <- rep(0,nreps) 
responses <- rep(0,nreps)
evidence <- matrix(0, nreps, nsamples+1) 
for (i in c(1:nreps)) { 
  evidence[i,] <- cumsum(c(0,rnorm(nsamples,drift,sdrw)))  
  p <-  which(abs(evidence[i,])>criterion)[1]
  responses[i] <- sign(evidence[i,p])
  latencies[i]  <- p
}
