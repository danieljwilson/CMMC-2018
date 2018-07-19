rm(list=ls())
library(rjags)
library(beanplot)
load.module('wiener')

load('dat.RData')

#Make a qp-plot for the data
qs=array(unlist(tapply(dat$rt,list(dat$correct,dat$cond,dat$pp),quantile,seq(.1,.9,.2))),dim=c(5,2,2,20))
qs.avg = array(apply(qs,1:3,mean),dim=c(5,4))

pCs=tapply(dat$correct,list(dat$cond,dat$pp),mean)
pCs.av = apply(pCs,1,mean)
matplot(c(1-pCs.av[1],pCs.av[1],1-pCs.av[2],pCs.av[2]),t(qs.avg),pch=15,xlim=c(0,1),col=1,ylab='Response Time',xlab='Accuracy')


###EZ###

#Function for EZ Diffusion Model
get.vaTer = function(Pc, VRT, MRT, s=.1){
  s2 = s^2
  if (Pc == 0)
    Pc=0.001
  if (Pc == 0.5)
    Pc=0.501
  if (Pc == 1)
    Pc=0.999
  L = qlogis(Pc)
  x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
  v = sign(Pc-0.5)*s*x^(1/4)
  a = s2*qlogis(Pc)/v
  y   = -v*a/s2
  MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
  Ter = MRT-MDT
  return(list(v, a, Ter))
}

#Set up some things
nsubjs=length(unique(dat$pp))
nconds=length(unique(dat$cond))
v = a = ter = array(dim=c(nsubjs,nconds))

#For each participant
for (sub in 1:nsubjs){
  #For each condition
  for (cond in 1:nconds){
    #Calculate the required inputs for get.vaTer (Pc, VRT, MRT)
    tmpdat = dat[dat$pp==sub&dat$cond==cond,]
    Pc = mean(tmpdat$correct)
    VRT = var(tmpdat$rt)
    MRT = mean(tmpdat$rt)

    #Use the get.vaTer function (i.e., the EZ function) to turn the data into diffusion model parameters
    tmp = get.vaTer(Pc = Pc, VRT = VRT, MRT = MRT)
    v[sub,cond] = tmp[[1]]
    a[sub,cond] = tmp[[2]]
    ter[sub,cond] = tmp[[3]]
  }
}

#Make a plot to see whether you think there are any differences in parameters across conditions
beanplot(v[,2],v[,1],a[,2],a[,1],ter[,2],ter[,1], col='grey',names=c('v1','v2','a1','a2','Ter1','Ter2'),at=c(1,2,4,5,7,8), xlim = c(0,9), what = c(F,T,T,T))
#Do a statistical test to check whether any differences are significant
t.test(v[,1],v[,2])
t.test(a[,1],a[,2])
t.test(ter[,1],ter[,2])


###JAGS###

jagsDat=dat 
#Write code to recode response times to be positive for one response, and negative for the other response
jagsDat$rt[jagsDat$correct==0]=-jagsDat$rt[jagsDat$correct==0]

#set up data
rts=jagsDat$rt
conds=jagsDat$cond
subjs=jagsDat$pp
N=length(rts)
  
data=list(rts=rts,conds=conds,subjs=subjs,nsubjs=nsubjs,N=N)
#set up model
model=jags.model('correct/wien-correct.txt',data=data,n.chains=3)
#burn-in
update(model,500)
#write code to indicate which parameters you want to save
parameters <- c('Alpha','Tau','Delta','sdAlpha','sdTau','sdDelta','prAlpha','prDelta','prTau')
#take samples from the model
samples <- coda.samples(model,variable.names=parameters,n.iter=2000)
#turn the output into something with which you can work!
samples <- as.matrix(samples)

#Collect the relevant parameter values
alphas=samples[,1:2]
deltas=samples[,3:4]
taus=samples[,5:6]

#Make a plot to see whether the parameters differ across conditions
layout(m=t(array(1:6,dim=c(3,2)))); par(mar=c(4,4,1,1))
beanplot(alphas[,1],alphas[,2],col='grey',names=c('AlphaA','AlphaB'),what = c(F,T,T,F)); 
beanplot(deltas[,1],deltas[,2],col='grey',names=c('DeltaA','DeltaB'),what = c(F,T,T,F)); 
beanplot(taus[,1],taus[,2],col='grey',names=c('TauA','TauB'), what = c(F,T,T,F));
beanplot(apply(alphas,1,diff),col='grey',names='Alpha',what = c(F,T,T,F));  
beanplot(apply(deltas,1,diff),col='grey',names='Delta',what = c(F,T,T,F)); 
beanplot(apply(taus,1,diff),col='grey',names='Tau',what = c(F,T,T,F)); 

#Make some inferences about which parameters differ across conditions
#collect priors
pralphas=samples[,7:8]
prdeltas=samples[,9:10]
prtaus=samples[,11:12]

#fit kernel density to work out height of posterior at 0
library(logspline)
tmpA=logspline(apply(alphas,1,diff))
tmpD=logspline(apply(deltas,1,diff))
tmpT=logspline(apply(taus,1,diff))

#fit kernel density to work out height of prior at 0  
tmpAPr=logspline(apply(pralphas,1,diff))
tmpDPr=logspline(apply(prdeltas,1,diff))
tmpTPr=logspline(apply(prtaus,1,diff))
  
#calculate Bayes factors by taking the ratio of these two quantities
bfA=dlogspline(fit=tmpA,0)/dlogspline(fit=tmpAPr,0)
bfD=dlogspline(fit=tmpD,0)/dlogspline(fit=tmpDPr,0)
bfT=dlogspline(fit=tmpT,0)/dlogspline(fit=tmpTPr,0)
