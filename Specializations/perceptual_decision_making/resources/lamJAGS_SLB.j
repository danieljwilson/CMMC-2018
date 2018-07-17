# JAGS implementation of the simple Linear Amplifier Model (single level Bayes).
# It receives the following data:
# jList=list("rootEnergy"=contrastMatrix, # 8 x 15 matrix (participants x contrast levels), filled with NA where no data are available
#           "n_per_condition"=nPerCondition, # 1 x 8 vector that contains the number of contrast levels sampled for each participant
#           "nCorrect"=nCorrect, # 8 x 15 matrix with number of correct decisions for each combination of participant and contrast
#           "nTrials"=nTotal, # 8 x 15 matrix with total number of trials for each combination of participant and contrast
#           "NsampIntegral"=100) # number of samples to use in evaluating numerical integral used to compute the predicted proportion correct

model{

	# constants
	sigma_int <- 1 # internal noise SD
	tau_int <- pow(sigma_int,-2) # internal noise precision
	mu_n <- 0 # mean noise response 
	maxBeta <- 5

	# set up a signal vector to integrate over - this is only relevant for the integration methods 1 and 2 in the loop below.
	mu_max <- maxBeta*3 # What is the biggest possible separation between signal and noise distributions (pick a reasonable value for the maximum root energy)?
	delta_x<-((mu_max+5*sigma_int)-(-5*sigma_int))/NsampIntegral # step size for integration
	x<-(-5*sigma_int)+(1:NsampIntegral)*delta_x # set up vector of signal values to integrate over
	x1<-x[1:(length(x)-1)]
	x2<-x[2:length(x)]

	for (subject in 1:length(n_per_condition)){
		
		beta[subject]~dnorm(0,1/2^2)T(0,maxBeta) # subject-level prior

		# Generate model predictions for each level of contrast
		for(i in 1:n_per_condition[subject]){
			# compute mean of internal response to the target
			mu_s[subject,i]<-beta[subject]*rootEnergy[subject,i]
			# For each signal x, compute the probability of the target response being larger than the three non-target responses
			for (j in 1:NsampIntegral){
				# Method 1: trapezoidal integration (j needs to go to (NsampIntegral-1)!)
				# pd[i,j]<-((dnorm(x2[j],mu_s[subject,i],tau_int)*(pnorm(x2[j],mu_n,tau_int)^3))+(dnorm(x1[j],mu_s[subject,i],tau_int)*(pnorm(x1[j],mu_n,tau_int)^3)))/2
				# Method 2: simply plug in some values and evaluate the relevant function
				pd[i,j]<-dnorm(x[j],mu_s[subject,i],tau_int)*pnorm(x[j],mu_n,tau_int)^3 # probability of all three noise responses being less than signal x
				# Method 3: use JAGS to simulate a load of outcomes and take the average (brute force)
				# target_response[i,j]~dnorm(mu_s[subject,i],tau_int) # simulate a target response
				# pd[i,j]<-pnorm(target_response[i,j],mu_n,tau_int)^3
			}
			PCpred[subject,i]<-sum(pd[i,])*delta_x # Probability correct computed under Methods 1 and 2
			#PCpred[subject,i]<-mean(pd[i,]) # Probability correct computed with the brute-force method 3
			nCorrect[subject,i]~dbin(PCpred[subject,i],nTrials[subject,i]) # Binomial likelihood of the observed number correct      
		}
	}

}