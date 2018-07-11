# Steven's law function

stevens_law <- function(stimuli, a, b){
  sub_int = a * stimuli^b
  return(sub_int)
}

stimuli <- c(2,4,6,8,10,12)

predictions <- stevens_law(stimuli, 1, 0.7)

# Simpler way to write the function
stevens_law2 <- function(x,a,b)
  
# Example observations
observations = c(1.1, 1.5, 2.1, 2.5, 2.7, 3.2)

# Plot observations against predictions
plot(stimuli, predictions, type = 'l', las=1, ylim=c(0, 7))
points(stimuli, observations, type='p', las=1, ylab="")

# Function to calculate the RMSD
rmsd = function (params, stimuli, observations){
  a = params[1]
  b = params[2]
  predictions = stevens_law(stimuli, a, b)
  rmsd_error = sqrt(mean((observations-predictions)^2))
  return(rmsd_error)
}

# Test out the above function
rmsd(c(0.5,.9), stimuli, observations)

# Minimize the rmsd using optim
results <- optim(c(2,1), rmsd, stimuli=stimuli, observations=observations)   
# stimuli = stimuli refers the stimuli variable in the rmsd function, and on the right is what we are passing
# in to that variable from our local environment

best_predictions <- stevens_law(stimuli, results$par[1], results$par[2])

# Plot observations against predictions
plot(stimuli, best_predictions, type = 'l', las=1, ylim=c(0, 4), col='red')
points(stimuli, observations, type='p', las=1, ylab="")

