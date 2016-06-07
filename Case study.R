###############################################################################################
###########     Case Study PartnerRE        ###################################################
###############################################################################################



#### Load Packages ############################################################################
library(xlsx)


#### Load Data ################################################################################
# The following code is used to load the data from different sources into the R environement

setwd("~")
Discount.Yield.Curve <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 1,header = T)
Mortality.Rates <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 2,header = T)
Policy.Records <- read.xlsx(file = "Desktop/Case Study/Case Study Data/Case study data.xlsx",sheetIndex = 3,header = T)






#### Policy Data ###############################################################################
# The Information about the policy data is implemented
Chosen.Policy.Records <- Policy.Records[1,]

# # Create variables with from the given information (in this case not actally needed)
# for(i in 1:4) assign(colnames(Chosen.Policy.Records)[i], as.numeric(Chosen.Policy.Records[i]))








#### Set Parameter #############################################################################
# the following code is used to set global parameters

Fixed.Expense <- 10
Profitability.Target <- 0.1

level.premium <- 20.5998784667068

# Calculating the years
projected.years <- 1:20
Year.Interval <- sapply(projected.years-1, function(i) paste0("[",i,",",i+1,"]"))
Policyholder.Age <- Chosen.Policy.Records$Policyholder.Age-1+projected.years







#### General Calculations  #############################################################
# Following are all the used functions to calculate the level Premium

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






#### Build data table #############################################################
# Binding the Information into one data.frame

# Projected Year
data.table.projected.year <- data.frame(c(0,projected.years))
colnames(data.table.projected.year) <- "projected year"

# Bind all the calculated values (ecxept Income and Outgo)
data.table.total <- data.frame(Year.Interval, Policyholder.Age, mortality.rate,policyholder.alive.start.year,policyholder.alive.end.year, expected.claims, expected.premium, expected.expenses, Discount.Yield.Curve[,2])
colnames(data.table.total) <- c("Year Interval","Policyholder Age","Mortality rate","Policyholders alive at start","Policyholders alive at end","Expected claims", "Expected premium", "Expected expenses", "Discount rate")

# Bind the above values and add projected year, Income and Outgo. Which all start one year earlier than the others
data.table.Income.Outgo <- data.frame(Income, Outgo)
colnames(data.table.Income.Outgo) <- c("Income","Outgo")

# Bind all the seperate dataframes to one full data frame
data.table <- data.frame(data.table.projected.year, rbind(rep(NA,ncol(data.table.total)),data.table.total), rbind(data.table.Income.Outgo, c(NA,NA)))








#### Write table #############################################################
# write the calculated tables als excel

Book <- createWorkbook()
Case.Study <- createSheet(wb=Book, sheetName="Case Study")
addDataFrame(x=data.table, sheet=Case.Study)
saveWorkbook(Book, "Desktop/Case Study/Case Study Data/data.table.xlsx")
















