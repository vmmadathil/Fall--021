library(fpp2)

setwd("C:/Users/madat/Fall 2021/Assignments/Forecasting/Exercise 3")

# Read in the data
hawaii <- ts(scan("Hawaii.csv"),frequency=12,start=c(1970,1))
# Print out the data in hawaii
hawaii
# plot the Hawaii data
ts.plot(hawaii,gpars=list(xlab="year",ylab="tourists", main="Hawaiian Tourism"))

# Buys-Ballot Plot - months-by-year seasonal plot

ggseasonplot(hawaii, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Tourists") +
  ggtitle("Seasonal Plot:  Hawaiian Tourism")


# Polar Seasonal Plot``

ggseasonplot(hawaii, polar=TRUE) +
  ylab("Tourists") +
  ggtitle("Polar Seasonal Plot: Hawaiian Tourism")

# Seasonal subseries plot: month-over-years plot
ggsubseriesplot(hawaii) +
  ylab("$Tourists") +
  ggtitle("Seasonal Subseries Plot: Hawiian Tourism")
