library("tidyverse")

# Let's first have a look at the help

dat <- {}

k <- 20
nsubj <- 10

# this is ugly; we could also use expand_grid
for (i in 1:nsubj){
  for (cond in c(1,2)){
    dat <- rbind(dat, 
                 cbind(rep(i,k),rep(cond,k),1:k,(1:k)+rnorm(k,0,0.5))
                 )
  }
}

dat <- data.frame(subj=factor(dat[,1]), cond=factor(dat[,2]), x = dat[,3], y=dat[,4])

# look at the data frame

# fit a regression line x ~ y for each combination of subject and condition
lmres <- dat %>% 
  group_by(subj,cond) %>%
  do(tfit = lm(y ~ x, data=.))
  
# two ways to get out the slopes
lmres %>% do(data.frame(coef = coef(.$tfit)[[1]]))

library("broom")
tidy(lmres, tfit) %>% 
  filter(term=="x")
glance(lmres, tfit)

# Exercise: Summarise the data. Calculate the mean, median, and SD of y for each participant in each condition. Use summarise() rather than do(). Look at the dplyr documentation for help

# Final note: this type of thing is great if you want to fit a model to each subject's data (ie the .fun function is your function to fit model to data, or e..g, obtain predictions given some parameter values)
