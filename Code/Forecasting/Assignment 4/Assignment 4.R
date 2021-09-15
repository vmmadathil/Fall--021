library(fpp2)

#setting working directory
setwd("C:/Users/madat/Fall 2021/Assignments/Forecasting/Exercise 3")

# Read in the data
hawaii <- ts(scan("Hawaii.csv"),frequency=12,start=c(1970,1))

# Viewing entire data set
hawaii

# Creating and Viewing the data partitions
# Training data set
hawaii1 <- head(hawaii, 12*25) 
hawaii1
# Test data set
hawaii2 <- tail(hawaii, 12) 
hawaii2


# Part A

## STL with window 5
hawaii1 %>%
  stl(t.window=5, s.window="periodic", robust=TRUE) %>%
  autoplot()

#STL with window 11
hawaii1 %>%
  stl(t.window=11, s.window="periodic", robust=TRUE) %>%
  autoplot()

#STL with window 21
hawaii1 %>%
  stl(t.window=21, s.window="periodic", robust=TRUE) %>%
  autoplot()



# Part B
#we are assuming that the appropriate window is 21

fit <- stl(hawaii1, t.window=21, s.window="periodic",
           robust=TRUE)

# Displaying the components of the stl decomposition
fit

# Here we are displaying the remainder separately.
remainder(fit)

res <- remainder(fit)
Acf(res)

#Box test
Box.test(res,lag=24,type=c("Ljung-Box"))



# Part C
# In this part of the program we look at three different
# stl forecasts, the three being "naive", "ets", and "arima".
# The forecasts are out-of-sample on the reserved (test) 15
# observations.

forc1 <- stlf(hawaii1, t.window=21, s.window="periodic",
              robust=TRUE, method="naive",h=15)
autoplot(forc1)
forc1

# Getting the forecasting accuracy measures for forc1
accuracy(forc1,hawaii2)

forc2 <- stlf(hawaii1, t.window=21, s.window="periodic", robust=TRUE, method="ets",h=15)

autoplot(forc2)
forc2

# Getting the forecasting accuracy measures for forc2
accuracy(forc2,hawaii2)

forc3 <- stlf(hawaii1, t.window=21, s.window="periodic",
              robust=TRUE, method="arima",h=15)
autoplot(forc3)
forc3

# Getting the forecasting accuracy measures for forc3
accuracy(forc3,hawaii2)

# Making matrix to display 3 forecasts and test observations.
mat <- c(forc1$mean,forc3$mean,hawaii2)
output <- matrix(mat,nrow=42,ncol=3,byrow=FALSE)

# Putting in the column names for the matrix
colnames(output) <- c("forc1","forc3","hawaii2")


# Part D

forc4 <- forecast(Arima(hawaii1,order=c(0,1,1), 
                        seasonal=c(0,1,1)),h=15)
accuracy(forc4,hawaii2)

summary(forc4)
