
# Author: David Kuchelmeister
#############################


#### Load Packages ############################################################################
library(xlsx)
library(DT)
require(parallel)
require(doMC)
library(compiler)


#### Set Parameter #############################################################################
# the following code is used to set global parameters

Fixed.Expense <- 10
Profitability.Target <- 0.1

level.premium <- 20.5998784667068

projected.years <- 1:20
Year.Interval <- sapply(projected.years-1, function(i) paste0("[",i,",",i+1,"]"))



##### optimise level of premium ##############################################

level.of.premium <- function(level.premium, Policy.Number){
  
  #### Policy Data ###############################################################################
  # The Information about the policy data is implemented
  Chosen.Policy.Records <- Policy.Records[Policy.Number,]
  
  
  #### Calculations of Level Premium #############################################################
  # Following are all the used functions to calculate the level Premium
  
  Policyholder.Age <- Chosen.Policy.Records$Policyholder.Age-1+projected.years
  
  # reading out the mortality rate of the current policy
  mortality.rate <- subset(Mortality.Rates$Rate, Mortality.Rates$Age %in% Policyholder.Age)
  
  # calculating percentage of alive policy holders over time
  policyholder.alive.end.year <- cumprod(1-mortality.rate)
  policyholder.alive.start.year <- c(1,head(policyholder.alive.end.year,-1))
  
  # Claims Expected To Be Paid Out to Policyholders (End of year timing)
  expected.claims <- mortality.rate * Chosen.Policy.Records$Sum.Assured.CHF * policyholder.alive.start.year
  
  # Premium Expected to be Received (Start of year timing)
  expected.premium <- policyholder.alive.start.year * level.premium
  
  # Expenses Expected (End of year timing)
  expected.expenses <- Fixed.Expense * policyholder.alive.end.year
  
  # Discount Rate
  Discount.Yield.Curve
  
  # Present Value of Income
  Income <- 0
  for(i in rev(projected.years)) Income <- c(Income, tail(Income,1)/(1+Discount.Yield.Curve$Rate[i]) + expected.premium[i])
  Income <- data.frame(rev(Income[-1])); colnames(Income) <- "Present Value of Income"
  
  # Present Value of Outgo
  Outgo <- 0
  for(i in rev(projected.years)) Outgo <- c(Outgo,  (expected.claims[i] + expected.expenses[i] + tail(Outgo,1)) / (1+Discount.Yield.Curve$Rate[i]))
  Outgo <- data.frame(rev(Outgo[-1])); colnames(Outgo) <- "Present Value of Outgo"
  
  # Calculate the level premium
  Actual.Profitability <- Income[1,]/Outgo[1,]-1
  
  # to optimise this function we want to minimise the difference between Actual.Profitability and Profitability.Target
  diff <- abs(Actual.Profitability - Profitability.Target)
  
  return(diff)
}
# cmpfun turn the function into aCompiling functions. (first compiling, then computing)
level.of.premium.compiler <- cmpfun(level.of.premium)

# level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium.compiler,c(0,100),tol = 0.01, i)$minimum
# level.premium <- Reduce(c,level.premium.list)