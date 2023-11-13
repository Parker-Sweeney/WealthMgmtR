## Program to compute CAPM for tickers in the file betas.csv and then optimize the portfolio
## File format  
##              Column 1: ticker
##              Column 2: betaVal
##  The rows are filled with each stock
##  Example:
## 	  ticker	betaVal
##   	MCD	    0.60
##   	MA	    1.13
##
##  John Lewis 12 October 2021
##  Updated 14 Oct 2021 to compute the tangent portfolio and the number of shares to buy
##  Updated 14 Feb 2022 to simplify the shares printout, select the beta file and simplify selection of  CAPM or Mean returns
##  Updated 16 Feb 2022 to handle stocks with a '-' in the name and remove stocks with less than 3 yrs of data
##
##  !!!! Must manually change the code to use mean return or CAPM expected returns 
##

library(quantmod)
library(timeSeries)
library(fPortfolio)
library(caTools)


#Turn off scientific notation
options(scipen=999)  #turn it back on with scipen=0

# Set type of expected returns
# CAPM = 1
# Mean Returns = 2 <- set as default
#
typeOfExpectedReturns <- 2

#set total size of the portfolio
portfolioAmount <- 1000000

# Both numbers below are 12 months out
SP500ExpectedReturn <- 0.07
riskFreeRate <- .0466


# Read the betas file.  It is read into a data frame
#betasFile <- read.csv("betas.csv",header=TRUE)

betasFileName <- file.choose(new = FALSE)
betasFile <- read.csv(betasFileName,header=TRUE)

# Convert tickers to string from factor
betasFile$ticker <- as.character(betasFile$ticker)

# Store the tickers in a tickerlist vector - as we have done in the past
tickerList <- betasFile$ticker

## Using just 3 years of history

##Download the stock prices
closingPricesRead <- NULL
for (ticker in tickerList)
  closingPricesRead <- cbind(closingPricesRead,
                             getSymbols(ticker, src="yahoo",from="2019-02-01", verbose=TRUE, auto.assign=FALSE)[,4]) # [,6] = keep the adjusted prices

names(closingPricesRead) <- gsub("\\..+","",names(closingPricesRead))  # remove ".Close" from names
betasFile$ticker <- gsub("\\..+","",betasFile$ticker)  # remove ".Close" from names
betasFile$ticker <- gsub("\\-.+","",betasFile$ticker)  # remove ".Close" from names

# keep only the dates that have closing prices for all tickers
# Delete by rows - will not work for all instances
# closingPrices <- closingPricesRead[apply(closingPricesRead,2,function(x) all(!is.na(x))),]
#  Delete by columns
closingPrices <- closingPricesRead[ , ! apply( closingPricesRead , 2 , function(x) any(is.na(x)) ) ]

#Update tickerList with names of just the tickers with full data
tickerList <- colnames(closingPrices)

#
if (nrow(betasFile) > length(tickerList)) {
  betasFileAdjusted <- betasFile[betasFile$ticker %in% tickerList,]
  message("Some of your stocks did not have enough data or odd characters and were deleted.")
} else {
  betasFileAdjusted <- betasFile
}

#tickerList <- betasFileAdjusted$ticker

# Compute the monthly returns
returns <- as.timeSeries((tail(closingPrices,-1) / as.numeric(head(closingPrices,-1)))-1)

meanReturns <- colMeans(returns)

#Compute expected returns from CAPM.  You have the betas and returns.  For example:
#
# To show the latest returns for my company GE, I would enter:
#tail(returns$GE.Close, n=1)  #n=1 forces R to display just the last value.  Otherwise, r will display the last 6 or so

# Since we are in R, we can compute the CAPM at one go using vectors and data frames
# and then store them in a new column in betasFile
betasFileAdjusted$ExpectedReturns <- riskFreeRate + betasFileAdjusted$betaVal * (SP500ExpectedReturn - riskFreeRate) 


# We could have put the results in a vector
expectedReturns <- riskFreeRate + betasFileAdjusted$betaVal * (SP500ExpectedReturn - riskFreeRate) 

# We can write out the results to a csv file
write.csv(betasFileAdjusted,"capm.csv")

#Create the estimator matrix for the optimization
covtEstimator <- function (x,data,spec) {
  x.mat = as.matrix(x)
  
  message("covtEstimator")

  if (typeOfExpectedReturns == 2) {
    
    #Using Means as expected Returns    
    list(mu=colMeans(x.mat),Sigma=MASS::cov.trob(x.mat)$cov)
    
  } else {
    
    #Use computed expected returns
    list(mu=expectedReturns,Sigma=MASS::cov.trob(x.mat)$cov)  
     
  }
    

}

#setup min and max constraints
minTransaction <- 0.5/nrow(betasFileAdjusted)
minTransactString <- paste("minW[1:length(tickerList)]=",as.character(minTransaction))
minMaxConstraints <- c(minTransactString,"maxW[1:length(tickerList)]=.30")


# Calculate Efficient Frontier with longonly constraints

defaultSpec <- portfolioSpec()
setRiskFreeRate(defaultSpec) <- riskFreeRate
setEstimator(defaultSpec) <- 'covtEstimator'
covtFrontier <- portfolioFrontier(returns, defaultSpec, constraints = "LongOnly")

#plot the efficient frontier
plot(covtFrontier,1)

#graph the weights of each security at each risk level computed
covtallocations <- getWeights(covtFrontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(covtallocations) <- tickerList
barplot(t(covtallocations), col=rainbow(ncol(covtallocations)+2), legend=colnames(covtallocations))

constraintsFrontier <- portfolioFrontier(returns, constraints = minMaxConstraints)

#plot the efficient frontier
plot(constraintsFrontier,1)

#graph the weights of each security at each risk level computed
constraintsAllocations <- getWeights(constraintsFrontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(constraintsAllocations) <- tickerList
barplot(t(constraintsAllocations), col=rainbow(ncol(constraintsAllocations)+2), legend=colnames(constraintsAllocations))


#display the correlations of the returns for the securities in the portfolio
cor(returns)

##
## Let's make life easier by computing the tangent portfolio (the only one in which we should invest)
## and compute the number of shares for each stock
##

#Now compute for the tangent portfolio
tangentPortfolio <- tangencyPortfolio(returns, defaultSpec, constraints = minMaxConstraints)


#compute the number of shares for each stock in the portfolio
tangentConstraintsShares <- (getWeights(tangentPortfolio) * portfolioAmount) / tail(closingPrices, n=1)


##
##Now compute the tangent portfolio without constraints
##
constraints <- "minW[1:length(tickerList)]=-1"  # Use this line to permit shorts
# Can you guess what will be in the next iteration 

tangentPortfolioLongOnly <- tangencyPortfolio(returns, defaultSpec,constraints = "LongOnly")


#compute the number of shares for each stock in the portfolio
tangentLongOnlyShares <- (getWeights(tangentPortfolioLongOnly) * portfolioAmount) / tail(closingPrices, n=1)


##Convert the xts objects to matrices so we can print them out
constraintsWeights <- as.matrix(as.data.frame(tangentConstraintsShares))
noConstraintsWeights <- as.matrix(as.data.frame(tangentLongOnlyShares))


##Print the number of shares to purchase for unconstrained portfolio and constrained portfolio

for (i in 1:length(tickerList)) {
  
  #Print the header the first time through the loop
  if( i == 1) {
    
    print(paste("Beta file: ",betasFileName ))
    print(" ")
    
    if (typeOfExpectedReturns == 1) {
      print("CAPM Expected Returns")
    } 
    if (typeOfExpectedReturns == 2) {
      print("Mean Returns as Expected Returns")
    }
    
    print("Count Stock   Shares No Constraints    Shares Constraints")
  }
  #print the shares  
  print(paste(i, "  ",tickerList[i],"       ",round(noConstraintsWeights[i]),"                   ",round(constraintsWeights[i])))
  
}

