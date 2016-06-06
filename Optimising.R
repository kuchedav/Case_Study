###############################################################################################
###########     Opmisisation Case Study PartnerRE        ######################################
###############################################################################################


#### Load Packages ############################################################################
library(xlsx)

#### Load Data ################################################################################
# The following code is used to load the data from different sources into the R environement

setwd("~")
Discount.Yield.Curve <- read.xlsx(file = "Desktop/Case Study/Case study data.xlsx",sheetIndex = 1,header = T)
Mortality.Rates <- read.xlsx(file = "Desktop/Case Study/Case study data.xlsx",sheetIndex = 2,header = T)
Policy.Records <- read.xlsx(file = "Desktop/Case Study/Case study data.xlsx",sheetIndex = 3,header = T)


# for(i in 1:3) Policy.Records <- rbind(Policy.Records,Policy.Records)
# Policy.Records$Policy.Number <- as.numeric(rownames(Policy.Records))




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





#### Optimising ####################### Finding Bottlenecks ################################################################################.

optimising.range <- c(0,130)
tolerance <- 0.01


# simple optimisation
sapply(Policy.Records$Policy.Number, function(i){
  optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum
})



# multicore calculation
require(parallel)
require(doMC)
registerDoMC(cores=detectCores())
level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum
(level.premium <- Reduce(c,level.premium))


# compiling functions
library(compiler)
level.of.premium.compiler <- cmpfun(level.of.premium)
level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% 
  optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum
(level.premium <- Reduce(c,level.premium.list))




# comaprison
library(microbenchmark)
compare <- microbenchmark(
  sapply(Policy.Records$Policy.Number, function(i){ optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum }),
  foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium,optimising.range,tol = tolerance, i)$minimum,
  foreach(i = Policy.Records$Policy.Number) %dopar% optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum
  ,times = 20
)

library(ggplot2)
plot(compare)








#### Finding Bottlenecks ################################################################################.

Rprof("out.out")

level.of.premium.compiler <- cmpfun(level.of.premium)
level.premium.list <- foreach(i = Policy.Records$Policy.Number) %dopar% 
  optimise(f = level.of.premium.compiler,optimising.range,tol = tolerance, i)$minimum
(level.premium <- Reduce(c,level.premium.list))

Rprof(NULL)
summaryRprof("out.out")



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

plot(Policy.Records$Policyholder.Age,level.premium)
abline(lm.erg, col="red4")



#### level premium / ~ #############################.

Analysis2 <- data.frame(Policy.Records$Policyholder.Age,level.premium)





