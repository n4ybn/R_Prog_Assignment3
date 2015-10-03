## rankall.R - starting over from scratch 2015-10-03
## by David DuPre

## The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
## containing the hospital in each state that has the ranking specified in num.

## rc <- setwd("C:/Downloads/Coursera/R_Prog_Assignment3")
rc <- setwd("C:/downloads/Coursera/R-Programming/R_Prog_Assignment3")
library(data.table)
library("sqldf")
path <- getwd()
histfilename <- paste0(path,"History","_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".R")
savehistory(histfilename)


rankall <- function(outcome, num = "best") {
        
        
        
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Column definitions of importance
        states          <- outcome[,7]
        hospitals       <- outcome[,2]
        HA              <- as.numeric(outcome[,11])
        HF              <- as.numeric(outcome[,17])
        PN              <- as.numeric(outcome[,23])

        ## Check that state and outcome are valid
        valid_states <- unique(outcome[,7])
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        
        ## Check for valid states   
        notvalidST=FALSE
        notvalidOUTCOME=FALSE
        
        for (i in valid_states) {
                if (i == state) {
                        notvalidST=TRUE
                }
        }
        
        ## Disable Warning
        options(warn=-1)
        
        ##   print(c("not valid =",notvalid))
        
        if (!notvalidST) {
                stop("invalid state",call.=TRUE)
        }
        
        if (!notvalidOUTCOME) {
                stop("invalid outcome",call.=TRUE)
        }
        
        ## Check for valid Outcome
        for (j in valid_outcomes) {
                if (j == oc) {
                        notvalidOUTCOME = TRUE
                }
        }
        
        ## For each state, find the hospital of the given rank
        for (i in valid_states) {
                i.hospital <- outcome[
                        
                ]
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}