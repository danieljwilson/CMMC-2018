# Jeff Rouder, 7/15/05


# This R code estimates the Rouder et al (2003, 2005)
# single-population Weibull model with rate parameterization.  To run
# it, you need file 'weibull-log-densities.R' No rights reserved.
# Feel free to use as needed.

# y_ij ~ Weibull(psi_i,lambda_i,beta_i)
# psi_i ~ uniform (0,min_j(y_ij))
# lambda_ij ~ gamma(xi1,xi2)
# beta_ij ~ gamma(eta1,eta2)
# xi1 ~ gamma(a1,b1)
# xi2 ~ gamma(a2,b2)
# eta1 ~ gamma(c1,d1)
# eta2 ~ gamma(c2,d2)


rm(list=ls()) #clear memory of any variables


#Step 0, load library and functions
library(HI)  #load adaptive rejectoin sampling (ARS) library
#if you get an error, you need to install HI library.  to do so,
#type: install.packages('HI')


source('weibull-log-densities.R') # load log-density for ARS calls



I=20 # number of participants
J=100 # number of items per participant

#Step 1, Simulate data.

#select true values
true.psi=runif(I,.2,.3) # generate psi from uniform (.2 sec, .3 sec)
true.lambda=exp(rnorm(I,3,.2)) # generate lambda from a log-normal
true.beta=runif(I,1.4,2.1) # generate beta from uniform (1.4,2.1)

#generate data
#most convenient to use shift,scale,shape parameterization
y=matrix(0,nrow=I,ncol=J)
for (i in 1:I)
{
true.theta=true.lambda[i]^(-1/true.beta[i])
y[i,]=rweibull(J,shape=true.beta[i],scale=true.theta)+true.psi[i]
}

# Look at a participant's data
#hist(y[1,])


# Step 2, Analysis

M=1000    #total number MCMC iterations
keep=101 #discard samples below as burn-in

#parameters

psi=matrix(nrow=M,ncol=I)
beta=matrix(nrow=M,ncol=I)
lambda=matrix(nrow=M,ncol=I)
xi1=1:M
xi2=1:M
eta1=1:M
eta2=1:M

#initial values
#to simulate analysis of real data, these should be reasonable, but
#far from true values
psi[1,]=apply(y,1,min)-.05 #for ea. participant, set psi as min(RT)-.05 sec
beta[1,]=rep(1.9,I) # for ea. participant, set beta=1.9
lambda[1,]=rep(12,I) # for ea. participant, set lambda=12
xi1[1]=.1
xi2[1]=.1
eta1[1]=.1
eta2[1]=.1

#hyperpriors on xi1,..,eta2
a1=2
b1=.1
a2=2
b2=2.85
c1=2
d1=.02
c2=2
d2=.04

#start main MCMC loop
for (m in 2:M)
{
#sample Weibull primary parameters psi,lambda,beta
for (i in 1:I)
{
	psi[m,i]=arms(psi[m-1,i],ld.psi,ind.psi,1,dat=y[i,],
              lambda=lambda[m-1,i],beta=beta[m-1,i])
	lambda[m,i]=rgamma(1,xi1[m-1]+J,xi2[m-1]+sum((y[i,]-psi[m,i])^beta[m-1,i]))
	beta[m,i]=arms(beta[m-1,i],ld.beta,ind.beta,1,dat=y[i,],
	       psi=psi[m,i],lambda=lambda[m,i],eta1=eta1[m-1],eta2=eta2[m-1])
}
xi1[m]=arms(xi1[m-1],ld.xi1,ind.xi1,1,lambda=lambda[m,],
	xi2=xi2[m-1],a1=a1,b1=b1)
xi2[m]=rgamma(1,a2+I*xi1[m],b2+sum(lambda[m,]))
eta1[m]=arms(eta1[m-1],ld.eta1,ind.eta1,1,beta=beta[m,],
	eta2=eta2[m-1],c1=c1,d1=d1)
eta2[m]=rgamma(1,c2+I*eta1[m],d2+sum(beta[m,]))
print(m)
}


# Step 3 plot posterior means vs. true values

est.psi=apply(psi[keep:M,],2,mean)
est.lambda=apply(lambda[keep:M,],2,mean)
est.beta=apply(beta[keep:M,],2,mean)

par(mfrow=c(2,2),mar=c(4,4,1,1))
plot(true.psi,est.psi)
abline(0,1)
plot(true.lambda,est.lambda)
abline(0,1)
plot(true.beta,est.beta)
abline(0,1)

