library(fpp2)

#setting working directory
setwd("C:/Users/madat/Fall 2021/Assignments/Forecasting/Exercise 3")

# Read in the data
hawaii <- ts(scan("Hawaii.csv"),frequency=12,start=c(1970,1))

# Creating and Viewing the data partitions
# Training data set
hawaii1 <- head(hawaii, 12*25) 
hawaii1
# Test data set
hawaii2 <- tail(hawaii, 12) 
hawaii2

# Simple Exponential Smoother (No Trend, No Seasonality)
fc1 <- ses(hawaii1,h=11)  
summary(fc1)
autoplot(fc1)
accuracy(fc1,hawaii2)

# Holt's Linear Trend Method (Trend, No Seasonality)

fc2 <- holt(hawaii1,h=11)
summary(fc2)
autoplot(fc2)
accuracy(fc2,hawaii2)
