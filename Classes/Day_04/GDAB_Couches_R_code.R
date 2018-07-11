rm(list=ls())
dev.off()

###STEVENS LAW

#FIRST write a function that produces Stevens Law predictions for any stimuli and parameters
#x = stimuli; a = first parameter; b = second parameter
stevens_fun1 <- function(x, a, b) {
  output = a*x^b
}
#Some stimuli (stimulus magnitudes) for the input
stimuli<-c(2, 4, 6, 8, 10, 12)

#Find predictions for a=1 and b=.07

#A simpler way to write the function
stevens_fun2 <- function(x, a, b) a*x^b

#Check it produces the same output
predictions2 <- stevens_fun2(stimuli, 1, 0.7)



####Draw a line graph of the data (observations) and predictions
plot(stimuli, predictions, type="l", las=1)

####Add points representing some experimental data (observations)
observations <-c(1.1, 1.5, 2.1, 2.5, 2.7, 3.2)
points(stimuli, observations, type="p", las=1, ylab="")

###Write a function that calculates RMSD (model error) given observations, stimuli, a, and b
#RMSD - Root Mean Squared Deviation

#We will input the parameters a and b as a two-element vector V
rmsd = function (V,stimuli,observations) {
  a=V[1]
  b=V[2]
  predictions <- stevens_fun1(stimuli, a, b) #Using the function we made
  rmsd_error=sqrt(mean((observations-predictions)^2)) #Done on whole vectors
  return(rmsd_error)
}

#Check to see if it works
rmsd(c(1,.7),stimuli,observations)

#Now we can use the function optim for the first time

#The first input to optim is a vector of "starting values" - here c(2,1)
#The second input is the function whose output is to be minimised - here rmsd
#The remaining inputs are the other inputs that rmsd needs
results<-optim(c(2,1),rmsd,stimuli=stimuli,observations=observations )

#The outcome is stored in "results" 
param_ests<-results$par #param_ests has the best-fit parameter estimates
error<-results$value    #smallest error that could be obtained 
param_ests
error


##NEXT plot the best-fit parameters and data on a new graph
dev.off() #Clear old plot

#First plot the data (observations)
plot(stimuli, observations, type="p",las=1)

#Now we need to calculate predictions with optim's estimated parameters
best_predictions <- stevens_fun1(stimuli, results$par[1], results$par[2]) 

#Now we can draw the graph
points(stimuli, best_predictions, type="l", col="red", las=1)

##############################################################
############# Now binary categorisation model ################
##############################################################
rm(list=ls())
dev.off()

#Provide the stimulus values of Category A items
CatA_vals=c(1,2,3,4,5)
#Provide the stimulus values of Category B items
CatB_vals=c(7,9,11,13,15)

#Now we need a function that takes any x value, and param c, and calculates the probability of 
#categorising x as category A

#We will write this as a function right away
categ = function (c, x) {
dists_to_A=abs(x-CatA_vals) #first calc the distances between x and all Cat A items
dists_to_B=abs(x-CatB_vals) #then distances between x and category B items
sims_to_A=exp(-c*dists_to_A) #turn distances into similarities
sims_to_B=exp(-c*dists_to_B) #turn distances into similarities
prob_A=sum(sims_to_A)/(sum(sims_to_A)+sum(sims_to_B)) 
return(prob_A)
}

#Check that it works
categ(0.5, 2)

#Now let us take several stimuli and (for each) return the probability
#that it will be classified as Category A
values=seq(1,30,.1)

prob_A=NULL #initalise a matrix that we will save results into
c=0.5 #the similarity-distance parameter
for (n in 1:length(values)){
prob_A[n]=categ(c,values[n])
}

#And plot the values
dev.off()
plot(values, prob_A, las=1)

#Look at a stimulus equidistant between the two categories (6)
#What can we conclude from the result?
categ(0.5, 6)




##############################################################
############# Now SIMPLE model of memory #####################
##############################################################
rm(list=ls())
dev.off()

c=6 #the similarity-distance parameter
retention_interval=1 #the retention interval

#First make a vector with the temporal distances of each item
#at the time of retrieval
temp_dists=c(10:1)

#Then add the retention interval
temp_dists=temp_dists+retention_interval

#Then log-transform the result
temp_dists=log(temp_dists)

#NOTE we could better have done all this in one line like this:
#temp_dists=log(c(10:1)+retention_interval)

#Now a loop that calculates discriminability
discrim=NULL                                # initalise a matrix that we will save results into

for (i in 1:length(temp_dists)){
  dist = abs(temp_dists[i]-temp_dists)        # find the summed distance from all 
  eta = exp(-c* dist)                         # compute  summed similarities
  discrim[i] = 1/sum(eta)                     # discriminability =inverse of summed similarities
}

#plot the results
dev.off()
plot(discrim, type="l",xlim=(c(0, 10)),ylim=((c(0,1))),ylab="Prob Correct",xlab="Serial Position",las=1)

#Take some data as recall probabilities(a serial position curve)
observations=c(8, 7, 6, 6, 8, 10, 10, 14, 14, 19)/20

#and plot them
points(observations, col="red")

#Now we want to create a function that will calculate a serial position curve for
#any value of c and and temporal distance vector
serpos = function (c,distances){
  for (i in 1:length(distances)){
    eta=exp(-c*abs(distances[i]-distances))  #summed sims
    discrim[i]=1/sum(eta) #discriminability   
  }
  output= discrim
}

#test the function
prediction=serpos(6, temp_dists)
prediction

#Now we want to create a function that will calculate the model error,
#given some data/observations
rmsd = function (c,distances,observations) {
  predictions=serpos(c, distances) #Existing function
  rmsd_error=sqrt(mean((observations-predictions)^2)) 
  return(rmsd_error)
}

#Check that it works as expected
error=rmsd(7,temp_dists,observations)
error

#Now we can use optimize to find best-fitting c value
#(because: only one parameter to estimate)
results=optimize(rmsd,c(0,50),distances=temp_dists,observations=observations)

#The outcome is stored in "results" 
param_ests<-results$minimum #param_ests has the best-fit parame estimates
error<-results$objective    #smallest error that could be obtained 
param_ests
error

#now we can plot the results
dev.off()
#First plot the data (observations)
plot(observations, type="p",xlim=(c(0, 10)),ylim=((c(0,1))),ylab="Prob Correct",xlab="Serial Position",las=1)

#Now we need to calculate predictions with optim's estimated parameters
best_predictions <- serpos(param_ests, temp_dists)  

#then we can add the points to the graph
points(best_predictions, type="l", col="green", las=1)


#####################################################
#########Now we can look at LOG LIKELIHOODS##########
#####################################################

#Now we have NUMBER CORRECT FROM 20 TRIALS
observations=c(8, 7, 6, 6, 8, 10, 10, 14, 14, 19)
ntrials<-20

#Illustrating dbinom

#probability of observing 8 recalls in 20 trials if
#predicted recall probability is 0.5
dbinom(8,ntrials,.5)

#We can do this with a whole vector of recall frequencies
#and a matching vector of predicted recall probs
dbinom(observations,ntrials,discrim)

#We can then log the probabilites, then sum them
#then take the negative
-sum(log(dbinom(observations,ntrials,discrim)))

#Now write a function to calculate LL
LL = function (c,N,distances,observations) {
  predictions=serpos(c, distances) #Using the function we made before
  LL_est=-sum(log(dbinom(observations,N,predictions)))
  LL_est=return(LL_est)
}




#now test it
LL(5,ntrials,temp_dists,observations)

#Now we need to use optimize to find maximum likelihood params
results<-optimize(LL,c(0,100),N=ntrials,distances=temp_dists,observations=observations)

#The outcome is again stored in "results" 
param_ests<-results$minimum #param_ests has the best-fit parame estimates
error<-results$objective    #smallest error that could be obtained 
param_ests
error

#Now add the curve onto the graph we already have:
#Now we need to calculate predictions with optim's estimated parameters
best_predictions <- serpos(param_ests, temp_dists)  

#then we can add the points to the graph
points(best_predictions, type="p", col="red", las=1)


