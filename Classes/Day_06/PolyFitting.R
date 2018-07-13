rm(list=ls())

# setwd("") # Set to the directory that contains 'PolyTrainData.csv' and 'PolyTestData.csv'

TrainData <- read.csv("PolyTrainData.csv",header=TRUE)

n <- 1 # Order of the polynomal to fit - play around with this to find what you think is the best-fitting model (based on plotting data and model fit in lines 11-15)

MyPolyFit <- lm(y~poly(x,n),data=TrainData)
plot(TrainData$x,TrainData$y)
xx <- seq(min(TrainData$x),max(TrainData$x),length.out=100)
yy <- predict(MyPolyFit,data.frame(x=xx))
lines(xx,yy,col="grey",lty=1,lwd=2)











# Now load in some "test data" and plot it against the model prediction
TestData <- read.csv("PolyTestData.csv",header=TRUE)
plot(TestData$x,TestData$y)
# Calculate the 'yy' model prediction
lines(xx,yy,col="grey",lty=1,lwd=2)