###############################################################################################
###########     Opmisisation Case Study PartnerRE        ######################################
###############################################################################################


#### Load Packages ############################################################################
library(xlsx)

#### Load Data ################################################################################
# The following code is used to load the data from different sources into the R environement

setwd("~")
Discount.Yield.Curve <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 1,header = T)
Mortality.Rates <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 2,header = T)
Policy.Records <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 3,header = T)








#### Set Parameter #############################################################################.
# the following code is used to set global parameters

Fixed.Expense <- 10
Profitability.Target <- 0.1

# Calculating the years
projected.years <- 1:20
Year.Interval <- sapply(projected.years-1, function(i) paste0("[",i,",",i+1,"]"))

Policy.Number <- 1

expected.premium <- 20.6



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
for(i in rev(projected.years)) Income <- c(Income, tail(Income,1)/(1+Discount.Yield.Curve$Rate[i]) + expected.premium)
Income <- data.frame(rev(Income[-1])); colnames(Income) <- "Present Value of Income"

# Present Value of Outgo
Outgo <- 0
for(i in rev(projected.years)) Outgo <- c(Outgo,  (expected.claims[i] + expected.expenses[i] + tail(Outgo,1)) / (1+Discount.Yield.Curve$Rate[i]))
Outgo <- data.frame(rev(Outgo[-1])); colnames(Outgo) <- "Present Value of Outgo"

# Calculate the level premium
Actual.Profitability <- Income[1,]/Outgo[1,]-1

Actual.Profitability








Actual.Profitability <- 0.1

Income.back <- (Actual.Profitability+1) * Outgo[1,]

Income.back





hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8
sh8 <- solve(h8)
round(sh8 %*% h8, 3)

A <- hilbert(4)
A[] <- as.complex(A)
## might not be supported on all platforms
try(solve(A))













{
  
  
  # Dennis Schnabel example 6.5.1 page 149
  dslnex <- function(x) {
    y <- numeric(2)
    y[1] <- x[1]^2 + x[2]^2 - 2
    y[2] <- exp(x[1]-1) + x[2]^3 - 2
    y
  }
  
  jacdsln <- function(x) {
    n <- length(x)
    Df <- matrix(numeric(n*n),n,n)
    Df[1,1] <- 2*x[1]
    Df[1,2] <- 2*x[2]
    Df[2,1] <- exp(x[1]-1)
    Df[2,2] <- 3*x[2]^2
    Df
  }
  
  BADjacdsln <- function(x) {
    n <- length(x)
    Df <- matrix(numeric(n*n),n,n)
    Df[1,1] <- 4*x[1]
    Df[1,2] <- 2*x[2]
    Df[2,1] <- exp(x[1]-1)
    Df[2,2] <- 5*x[2]^2
    Df
  }
  
  test <- function(x){
    x[2] <- (344.9809 - policyholder.alive.start.year[1] * x[1]) * (1 + Discount.Yield.Curve[1,2])
    x[3] <- (x[2] - policyholder.alive.start.year[2] * x[1]) * (1 + Discount.Yield.Curve[2,2])
    x[4] <- (x[3] - policyholder.alive.start.year[3] * x[1]) * (1 + Discount.Yield.Curve[3,2])
    x[5] <- (x[4] - policyholder.alive.start.year[4] * x[1]) * (1 + Discount.Yield.Curve[4,2])
    x[6] <- (x[5] - policyholder.alive.start.year[5] * x[1]) * (1 + Discount.Yield.Curve[5,2])
    x[7] <- (x[6] - policyholder.alive.start.year[6] * x[1]) * (1 + Discount.Yield.Curve[6,2])
    x[8] <- (x[7] - policyholder.alive.start.year[7] * x[1]) * (1 + Discount.Yield.Curve[7,2])
    x[9] <- (x[8] - policyholder.alive.start.year[8] * x[1]) * (1 + Discount.Yield.Curve[8,2])
    x[10] <- (x[9] - policyholder.alive.start.year[9] * x[1]) * (1 + Discount.Yield.Curve[9,2])
    x[11] <- (x[10] - policyholder.alive.start.year[10] * x[1]) * (1 + Discount.Yield.Curve[10,2])
    x[12] <- (x[11] - policyholder.alive.start.year[11] * x[1]) * (1 + Discount.Yield.Curve[11,2])
    x[13] <- (x[12] - policyholder.alive.start.year[12] * x[1]) * (1 + Discount.Yield.Curve[12,2])
    x[14] <- (x[13] - policyholder.alive.start.year[13] * x[1]) * (1 + Discount.Yield.Curve[13,2])
    x[15] <- (x[14] - policyholder.alive.start.year[14] * x[1]) * (1 + Discount.Yield.Curve[14,2])
    x[16] <- (x[15] - policyholder.alive.start.year[15] * x[1]) * (1 + Discount.Yield.Curve[15,2])
    x[17] <- (x[16] - policyholder.alive.start.year[16] * x[1]) * (1 + Discount.Yield.Curve[16,2])
    x[18] <- (x[17] - policyholder.alive.start.year[17] * x[1]) * (1 + Discount.Yield.Curve[17,2])
    x[19] <- (x[18] - policyholder.alive.start.year[18] * x[1]) * (1 + Discount.Yield.Curve[18,2])
    x[20] <- (x[19] - policyholder.alive.start.year[19] * x[1]) * (1 + Discount.Yield.Curve[19,2])
    x
  }
  
  # xstart <- rep(1,20)
  xstart <- c(2,0.5)
  fstart <- dslnex(xstart)
  xstart
  fstart
  
  # a solution is c(1,1)
  nleqslv(xstart, test, control=list(btol=.01))
  
  # Cauchy start
  nleqslv(xstart, dslnex, control=list(trace=1,btol=.01,delta="cauchy"))
  
  # Newton start
  nleqslv(xstart, dslnex, control=list(trace=1,btol=.01,delta="newton"))
  
  # final Broyden approximation of Jacobian (quite good)
  z <- nleqslv(xstart, dslnex, jacobian=TRUE,control=list(btol=.01))
  z$x
  z$jac
  jacdsln(z$x)
  # different initial start; not a very good final approximation
  xstart <- c(0.5,2)
  z <- nleqslv(xstart, dslnex, jacobian=TRUE,control=list(btol=.01))
  z$x
  z$jac
  jacdsln(z$x)
  ## Not run:
  # no global strategy but limit stepsize
  # but look carefully: a different solution is found
  nleqslv(xstart, dslnex, method="Newton", global="none", control=list(trace=1,stepmax=5))
  # but if the stepsize is limited even more the c(1,1) solution is found
  # 
  # 
  # 
}


