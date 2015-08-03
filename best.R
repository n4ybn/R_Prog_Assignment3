## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
library(dplyr)
library(sqldf)
## library(tidyr)
## rc <- setwd("C:/downloads/Coursera/R_Prog_Assignment3")
rc <- setwd("C:/downloads/Coursera/R-Programming/R_Prog_Assignment3")

best <- function (state,oc){
        
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

        ## Setup what is valid        
        valid_states <- unique(outcome[,7])
        valid_outcomes <- c("heart attack","pneumonia","heart failure")

        ## Check that the State and outcome are valid
        notvalidST=FALSE
        notvalidOUTCOME=FALSE
        
        ## Check for valid states   
        for (i in valid_states) {
                if (i == state) {
                        notvalidST=TRUE
                }
        }
        ## Check for valid Outcome
        for (j in valid_outcomes) {
                if (j == oc) {
                        notvalidOUTCOME = TRUE
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
        
        ## Re-enable Warnings
        options(warn=0)        
                
        ## Get names of Hospitals
        Hospitals <- outcome[,2]
        States <- outcome[,7]
        
        ## Reduce rows to Single State
        ocBS <- outcome[grep(state,outcome$State),]
        
        sorted_obs <- ocBS[with (ocBS, order(ocBS[,15],ocBS[,2])),]
        print(sorted_obs[1,2])
       

        
        ## Begin the processing of the Data Frames for each valid type
       ## print("DID NOT STOP")
        
        ## Return hospital name in that state with lowest 30-day death rate
        
}