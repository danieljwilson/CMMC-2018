# functions for ARS calls of Weibull single-population rate model
# Jeff Rouder, 7/15/05

ld.psi=function(x,dat,lambda,beta)   # log of conditional posterior density for psi
(beta-1)*sum(log(dat-x))-
lambda*(sum((dat-x)^beta))

ind.psi=function(x,dat,lambda,beta) # range of psi
(x>0)*(x<min(dat))

ld.beta=function(x,dat,psi,lambda,eta1,eta2)   # log of conditional posterior density for beta
(J+eta1-1)*log(x)-
lambda*sum((dat-psi)^x)-
x*eta2+x*sum(log(dat-psi))

ind.beta=function(x,dat,psi,lambda,eta1,eta2)  # range of beta
(x>.01)*(x<10)

ld.xi1=function(x,lambda,xi2,a1,b1)   # log of conditional posterior density for xi1
{
x*(I*log(xi2)+sum(log(lambda)))-
I*lgamma(x)+
(a1-1)*log(x)-
b1*x
}

ind.xi1=function(x,lambda,xi2,a1,b1)  # range of xi1
(x>.01)*(x<10)

ld.eta1=function(x,beta,eta2,c1,d1)   # log of conditional posterior density for eta1
{
x*(I*log(eta2)+sum(log(beta)))-
I*lgamma(x)+
(c1-1)*log(x)-
d1*x
}

ind.eta1=function(x,beta,eta2,c1,d1)  # range of eta1
(x>.01)*(x<10)

