dev.off() # clear the graphs

# Some of the following is from Gordon's bit on psychophysics

#Some stimuli (stimulus magnitudes) for the input
stimuli<-c(2, 4, 6, 8, 10, 12)

#points representing some experimental data (observations)
observations <-c(1.1, 1.5, 2.1, 2.5, 2.7, 3.2)
n <- length(observations)

# fit a model with a polynomial of degree one less than the number of observations
# poly takes independent variable (stimuolus) and creates independent predictor variables
lm_n <- lm(observations ~ poly(stimuli,n-1))

plot(stimuli,observations, pch=3, las=1,
     xlab='Physical Intensity',
     ylab='Perceived Intensity')
lines(stimuli,predict(lm_n))

# Exercise: generate observations from a variety of functions (with variability), and fit polynomial as above

# Example:
observations <- log(stimuli) + rnorm(n,0,0.1)
observations <- exp(stimuli/4) + rnorm(n, 0, 2)
observations <- exp(stimuli/4) + runif(n, 0, 2)
observations <- stimuli^2 - stimuli^3 + rnorm(n, -5, 5)
observations <- runif(n,2,10) + exp(-stimuli) + rgeom(n, .1) * rbinom(n, 10,.1) + rnorm(n,2,12) 

# Things to try: different amounts of noise, linear fn, exponential, decreasing function, quadratic, sine...

