# JAGS implementation of the simple Linear Amplifier Model (hierarchical/multi-level approach).
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

	# Set up a signal vector to integrate over (we need this in order to compute a precited probability correct).
	mu_max <- maxBeta*3 # What is the biggest possible separation between signal and noise distributions (pick a reasonable value for the maximum root energy)?
	delta_x<-((mu_max+5*sigma_int)-(-5*sigma_int))/NsampIntegral # step size for integration
	x<-(-5*sigma_int)+(1:NsampIntegral)*delta_x # set up vector of signal values to integrate over

	for (subject in 1:length(n_per_condition)){
		
		beta[subject]~dnorm(mu_beta,tau_beta) # subject-level prior

		# Generate model predictions for each level of contrast
		for(i in 1:n_per_condition[subject]){
			# compute mean of internal response to the target
			mu_s[subject,i]<-beta[subject]*rootEnergy[subject,i]
			# Compute the probability of the target response being larger than the three non-target responses			
			PCpred[subject,i]<-inprod(dnorm(x,mu_s[subject,i],tau_int),pnorm(x,mu_n,tau_int)^3)*delta_x
			nCorrect[subject,i]~dbin(PCpred[subject,i],nTrials[subject,i]) # Binomial likelihood of the observed number correct      
		}
	}
	
	# Parent distributions
	mu_beta~dunif(0,10)
	tau_beta ~ dgamma(0.001,0.001)
}