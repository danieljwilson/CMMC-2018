# In order to get JAGS to work:
# (1) Install the latest version of JAGS from sourceforge (e.g., 
# http://sourceforge.net/projects/mcmc-jags/files/)
# (2) Install the latest version of rjags. Do **not** try to install this via the
# usual route (i.e., R -> Packages -> Install package(s)) as this may install
# an old version of rjags that expects an old version of JAGS.
# Instead, you want to Google for rjags CRAN, go to a site 
# such as http://cran.r-project.org/web/packages/rjags/index.html, and --when
# using Windows-- download the .zip file. Then, in R, go to Packages ->
# Install package(s) from local zip file...
# To check, type library(rjags) at the R prompt.
# (3) Install R2jags. This does work via the usual route 
# R -> Packages -> Install package(s).
# install.packages("R2jags")
# Now you are all set.

devtools::install_url("https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.5/rjags_4-6.tgz",
                      args="--configure-args='--with-jags-include=/Users/djw/homebrew/opt/jags/include/JAGS
                      --with-jags-lib=/Users/djw/homebrew/opt/jags/lib'
                      "
)

# clears workspace:  
rm(list=ls()) 

# sets working directories:
#setwd("/Users/jennifer/Dropbox/Documents/Teaching/CognitiveModelingSummerSchool2018/BayesianModeling/Codes")

library(R2jags)

k <- 9
n <- 10

data <- list("k", "n") # to be passed on to JAGS

myinits <-	list(
  list(theta = 0.1), #chain 1 starting value
  list(theta = 0.9)) #chain 2 starting value

# parameters to be monitored:	
parameters <- c("theta")

# The following command calls JAGS with specific options.
# For a detailed description see the R2jags documentation.
samples <- jags(data, inits=myinits, parameters,
	 			  model.file ="Rate_1.txt", n.chains=2, n.iter=20000, 
          n.burnin=1, n.thin=1, DIC=T)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

# The command below is useful for a quick overview:
print(samples)  # a rough summary

# Collect posterior samples across all chains:
theta <- samples$BUGSoutput$sims.list$theta
 
# Now let's plot a histogram for theta. 
Nbreaks <- 80
y       <- hist(theta, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=1,
     xlim=c(0,1), xlab="Rate", ylab="Posterior Density") 