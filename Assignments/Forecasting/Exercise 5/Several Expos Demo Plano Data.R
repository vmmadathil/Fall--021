library(fpp2)

# Read in the Plano Data.  You may have change the below directory
# location to match where your data is actually located. 
plano <- ts(scan("D:/Data/Plano.csv"),frequency=12,start=c(1990,2))
# Print out the data
plano
# Plot the data
autoplot(plano)

# Create the training and test data sets.
plano2 <- window(plano,start=c(1990,2), end=c(2004,12))
plano3 <- window(plano,start=c(2005,1), end=c(2005,11))

plano2 # training data set
plano3 # test data set

# Here we are going to try 7 different exponential smoothers
# on the Plano data that are discussed in Chapter 7 of FPP2
# and see which methods provides the most accurate forecasts
# in the test data set.

# Simple Exponential Smoother (No Trend, No Seasonality)

fc1 <- ses(plano2,h=11)  
summary(fc1)
autoplot(fc1)
accuracy(fc1,plano3)

# Holt's Linear Trend Method (Trend, No Seasonality)

fc2 <- holt(plano2,h=11)
summary(fc2)
autoplot(fc2)
accuracy(fc2,plano3)

# Holt's Damped Linear Trend Method (Damped Trend,
# No Seasonality)

fc3 <- holt(plano2,damped=TRUE,h=11)
summary(fc3)
autoplot(fc3)
accuracy(fc3,plano3)

# Holt-Winter's Additive Seasonal Method (Trend, Additive
# Seasonality)

fc4 <- hw(plano2,seasonal="additive",h=11)
summary(fc4)
autoplot(fc4)
accuracy(fc4,plano3)

# Holt-Winter's Additive Seasonal Method with Damping
# (Damped Trend, Additive Seasonality)

fc5 <- hw(plano2,seasonal="additive",damped=TRUE,h=11)
summary(fc5)
autoplot(fc5)
accuracy(fc5,plano3)

# Holt-Winter's Multiplicative Seasonal Method (Trend,
# Multiplicative Seasonality)

fc6 <- hw(plano2,seasonal="multiplicative",h=11)
summary(fc6)
autoplot(fc6)
accuracy(fc6,plano3)

# Holt-Winter's Multiplicative Seasonal Method with Damping
# (Trend, Multiplicative Seasonality)
fc7 <- hw(plano2,seasonal="multiplicative",damped=TRUE,h=11)
summary(fc7)
autoplot(fc7)
accuracy(fc7,plano3)
