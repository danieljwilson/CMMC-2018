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
get.vaTer = function(Pc, VRT, MRT, s=.1){  #Pc = portion of correct responses, VRT = variance in rt
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
  MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))    # mean decision time
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
#Make inferences as to whether there are differences across conditions
t.test(v[,1]-v[,2])
t.test(a[,1]-a[,2])
t.test(ter[,1]-ter[,2])

