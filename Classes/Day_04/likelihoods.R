# Monte carlo simulation of coin flipping

nGames <- 1000
pHeads <- 0.7
nTosses <- 8

res <- rep(0, nGames)
for (rep in 1:nGames){ # using a loop to make this obvious!
  res[rep] <- sum((runif(nTosses)<pHeads))
}

hist(res, 
     xlab="number of heads",
     freq=FALSE,
     breaks=(c(0:9)-0.5))





# next bit puts predictions on top--for later
k <- 0:nTosses
pK <- dbinom(k, nTosses, pHeads)
lines(k, pK, type="h", col="red",
     xlab="Number of heads",
     ylab="Probability")












# plot using binomial
nTosses <- 10
k <- 0:nTosses
pHeads <- 0.5
pK <- dbinom(k, nTosses, pHeads)
plot(k, pK, type="h",
     xlab="Number of heads",
     ylab="Probability")












# SIMPLE data probability
pData <- dbinom(5, 8, 0.7)








x <- seq(0,1000,length.out = 100)
scale <- 200
shape <- 2
plot(x, dweibull(x,shape = shape, scale=scale),
     type="l",
     xlab="RT (ms)",
     ylab="Prob Density")

dweibull(200,shape = shape, scale=scale)











## Maximum likelihood estimation of some data
rt <- scan("rt.txt")

weiblnL <- function(theta, rt){
  
  shift <- theta[1]
  shape <- theta[2]
  scale <- theta[3]
  
  if (shift>=min(rt) | any(theta<.Machine$double.neg.eps) ){
    return(1000000)
  } else {
    return(
      -sum(dweibull(rt-shift,scale=scale,shape=shape,log=TRUE))
    )
  }
}

# parameters are shift, scale, shape
fit <- optim(c(1,1.8,mean(rt)),
             weiblnL, rt=rt)

hist(rt, probability = TRUE, breaks=20)
lines(0:1000, dweibull((0:1000)-fit$par[1],shape=fit$par[2], scale=fit$par[3]), col=3, lwd=2)



















# Answer
library(plyr)

parms <- ddply(dat,.variables = .(Subject),
               .fun=function(x){
                 fit <- optim(c(min(rt)/2,200,2),
                              weiblnL, rt=x$FixTime[x$SaccAcc==1])
                 return(data.frame(shift=fit$par[1],
                                   scale=fit$par[2],
                                   shape=fit$par[3],
                                  converge=fit$convergence))
               })

# But this gives us some numerical problems. Express RT as seconds
library(plyr)

parms <- ddply(dat,.variables = .(Subject),
               .fun=function(x){
                 fit <- optim(c(0,mean(rt/1000),2),
                              weiblnL, rt=x$FixTime[x$SaccAcc==1]/1000)
                 return(data.frame(shift=fit$par[1],
                                   scale=fit$par[2],
                                   shape=fit$par[3],
                                   converge=fit$convergence))
               })
