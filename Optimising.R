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






##### optimise level of premium ##############################################.
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





#### Set Parameter #############################################################################.
# the following code is used to set global parameters

Fixed.Expense <- 10
Profitability.Target <- 0.1

# Calculating the years
projected.years <- 1:20
Year.Interval <- sapply(projected.years-1, function(i) paste0("[",i,",",i+1,"]"))











#### Optimisation #####################################################################################################.
# to be able to optimise, we need to know how we should optimise
# 
# It would be possible to calculate the optimas with a equation-system, but R can not calculate analytically.
# Therefore, normal optimisations are probably the best choice
# 
# We have some different possibilities for optimisation codes and try to find the fastest possible



#### Optimisation methods #####################################
# To find the fastest optimisation, we need to
#   calculate all the possibilites we have and compare them

# The apply-loop is added to calculate the optimas multiple times
#  this may take a while but the time predictino is more accurate 
#  This apply-loop is not built to be specifically fast, because it will be run only once.
calc.time.table <- sapply(1:2, function(i){
  
  
  
  ## 1. optim()
  # optim() offers 6 different methods, which we will all run trough
  methods <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")
  method.list <- lapply(methods, function(j){
    info <- list()
    start.time <- Sys.time()
    info$values <- sapply(1: nrow(Policy.Records), function(i) {
      optim(par= 40, fn= level.of.premium, Policy.Number= i, method= j, lower= 0, upper= 200)$par
    })
    info$time.difference <- Sys.time()-start.time
    info
  })
  names(method.list) <- methods
  calc.time <- sapply(1:length(method.list), function(i) as.numeric(method.list[[i]][2]))
  
  
  
  ## 2. optimise()
  # optmise() only has one mehtod
  optimising.range <- c(0,130)
  tolerance <- 0.01
  
  start.time <- Sys.time()
  sapply(1: nrow(Policy.Records), function(i) {
    optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum })
  time.difference <- Sys.time()-start.time
  
  # Add the optimise method to the others
  calc.time <- c(calc.time,time.difference)
  methods <- c(methods,"optim")
  calc.time
  
  
  
}) # end of apply-loop




## calculate the mean over the different cases
calc.time.mean <- rowMeans(calc.time.table)


## Plot the comparison of the calculation speeds
library(plotly)
m = list( l = 110, r = 150, b = 0, t = 100, pad = 4 )
plot_ly(x=calc.time.mean, y=methods, mode = "markers", marker = list(color = "blue")) %>%
  layout( autosize = F, margin = m, title = "Calculation speed per method",
          yaxis = list(title = "Method"), xaxis = list(title = "Calculation time"))

# We conculte from the graphic, that optim() has the best performance





#### Optimising loops ###############################################
# The optimisation needs to run over multiple policies, for which we use a loop
# this loop can be optimised with different methods, which we will compare



## simple optimisation
sapply(Policy.Records$Policy.Number, function(i){
  optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum
})


## multicore calculation (foreach-loop)
# a foreeach.loop distributes tasks over all available cores on the computer
require(parallel)
require(doMC)
registerDoMC(cores=detectCores())
level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum
(level.premium <- Reduce(c,level.premium.list))



## compiling functions
# R is a programmlanguage, where the code is computed, as it is compiled
# with cmpfun we manipulate functions so that they are first compiled and the computed (just as in C++)
library(compiler)
level.of.premium.compiler <- cmpfun(level.of.premium)
level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% 
  optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum
(level.premium <- Reduce(c,level.premium.list))






## comaprison
library(microbenchmark)
compare <- microbenchmark(
  sapply(Policy.Records$Policy.Number, function(i){ optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum }),
  foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum,
  foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum
  ,times = 20
)

library(ggplot2)
plot(compare,col="lightblue")








#### Find Bottlenecks ################################################################################.

Rprof("out.out")

level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% 
  optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum

Rprof(NULL)
summaryRprof("out.out")


# different analysation
proftable <- function(file, lines = 10) {
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  filelines <- grep("#File", profdata)
  files <- profdata[filelines]
  profdata <- profdata[-c(1, filelines)]
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  colnames(stacktable) <- c("PctTime", "Call")
  stacktable <- head(stacktable, lines)
  shortcalls = strsplit(stacktable$Call, " > ")
  shortcalls.len <- range(sapply(shortcalls, length))
  parent.call <- unlist(lapply(seq(shortcalls.len[1]), function(i) Reduce(intersect, lapply(shortcalls,"[[", i))))
  shortcalls <- lapply(shortcalls, function(x) setdiff(x, parent.call))
  stacktable$Call = sapply(shortcalls, function(x) paste(x, collapse = " > "))
  if (length(parent.call) > 0) {
    parent.call <- paste(paste(parent.call, collapse = " > "), "> ...")
  } else {
    parent.call <- "None"
  }
  frac <- sum(stacktable$PctTime)
  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  if(length(files) > 0) {
    cat("\n")
    cat(paste(files, collapse="\n"))
    cat("\n")
  }
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
  
  invisible(stacktable)
}
proftable("out.out")
















#### Analysation ################################################################################.


source("Desktop/Case Study/Case study.R")

#### level premium / Age #############################.

Analysis1 <- data.frame(Policy.Records$Policyholder.Age,level.premium)

# Regression
lm.erg <- lm(level.premium ~ Policy.Records.Policyholder.Age, data=Analysis1)
summary(lm.erg)

plot(Policy.Records$Policyholder.Age,level.premium, xlim= c(0,70), ylim= c(0,120))
# abline(lm.erg, col="red4")
for(i in 8:12) lines(smooth.spline(x=Policy.Records$Policyholder.Age,y=level.premium,spar = 0.6),col=2)
lines(predict(smooth.spline(x=Policy.Records$Policyholder.Age,y=level.premium,spar = 0.6),1:20))
lines(predict(smooth.spline(x=Policy.Records$Policyholder.Age,y=level.premium,spar = 0.6),60:70))



plot(log(Mortality.Rates),type="l")
















