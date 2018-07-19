rm(list=ls())
library(rjags)
load.module('wiener')

load('dat.RData')

#Make a qp-plot for the data
qs=array(unlist(tapply(dat$rt,list(dat$correct,dat$cond,dat$pp),quantile,seq(.1,.9,.2))),dim=c(5,2,2,20))
qs.avg = array(apply(qs,1:3,mean),dim=c(5,4))

pCs=tapply(dat$correct,list(dat$cond,dat$pp),mean)
pCs.av = apply(pCs,1,mean)

matplot(c(1-pCs.av[1],pCs.av[1],1-pCs.av[2],pCs.av[2]),t(qs.avg),pch=15,xlim=c(0,1),col=1,ylab='Response Time',xlab='Accuracy')

#Set up some things
nsubjs=length(unique(dat$pp))
nconds=length(unique(dat$cond))

###JAGS###
jagsDat=dat
jagsDat$rt[jagsDat$correct==0] = -jagsDat$rt[jagsDat$correct==0]

#set up data
rts=jagsDat$rt
conds=jagsDat$cond
subjs=jagsDat$pp
N=length(rts)

data=list(rts=rts,conds=conds,subjs=subjs,nsubjs=nsubjs,N=N)
#set up model
model=jags.model('wien-simple.txt',data=data,n.chains=3)
#burn-in
update(model,500)
#write code to indicate which parameters you want to save
parameters = c('alpha','tau','delta')
#take samples from the model
samples <- coda.samples(model,variable.names=parameters,n.iter=2000)
#turn the output into something with which you can work!
samples <- as.matrix(samples)

#Collect the relevant parameter values
alpha = samples[,'alpha']
tau = samples[,'tau']
delta1 = samples[,'delta[1]']
delta2 = samples[,'delta[2]']

#Make some inferences about which parameters differ across conditions
t.test(v[,1]-v[,2])
t.test(a[,1]-a[,2])
t.test(ter[,1]-ter[,2])

#Plot
#Make a plot to see whether you think there are any differences in parameters across conditions
beanplot(v[,2],v[,1],a[,2],a[,1],ter[,2],ter[,1],
         col='grey',
         names=c('v1','v2','a1','a2','Ter1','Ter2'),
         at=c(1,2,4,5,7,8), 
         xlim = c(0,9), 
         what = c(F,T,T,T))