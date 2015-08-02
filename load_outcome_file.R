setwd("C:/Downloads/Coursera/R_Prog_Assignment3")
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
## head(outcome)

## Row count
nr <- nrow(outcome)
## Column Count
nc <- ncol(outcome)
## List of names of Columns
cn <- names(outcome)

## Make a histogram of the 30 day death rates

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])