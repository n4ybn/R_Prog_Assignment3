## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
## rc <- setwd("C:/Downloads/Coursera/R_Prog_Assignment3")
## rc <- setwd("C:/downloads/Coursera/R-Programming/R_Prog_Assignment3")
## library(dplyr)
## library(tidyr)

## SET dbug to TRUE to enable DEBUG code, FALES to disable it.
dbug <- FALSE

best <- function (state,oc){

        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that the State and outcome are valid
        
        ## Load valid state codes from complete file.
        valid_states <- unique(outcome[,7])
        
        ## Load valid hospitals from complete file.
        Hospitals <- outcome[,2]
        
        ## Hardcode the valid Outcomes
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        
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
        
        ## Disable Warning from conversions to numeric causing NAs.
        options(warn=-1)
        
        ##   print(c("not valid =",notvalid))
        
        if (!notvalidST) {
                stop("invalid state",call.=TRUE)
        }
        
        if (!notvalidOUTCOME) {
                stop("invalid outcome",call.=TRUE)
        }
        
        ## Reduce outcome to contain rows limited to the State selected
        outcome <- outcome[outcome$State==state,]
        
        ## Convert the columns from STRING to numeric
        outcome[,11] <- as.numeric(outcome[,11])  ## HA
        outcome[,17] <- as.numeric(outcome[,17])  ## HF
        outcome[,23] <- as.numeric(outcome[,23])  ## PN
        
        ## Re-enable Warnings   
        options(warn=0)
        
        ## Get list of Hospitals for this State
        Hospitals <- outcome[,2]
        
        ## Build DF of HA,HF,PN,Hospitals for this State.
        reportdata <- data.frame(outcome[,11],outcome[,17],outcome[,23],outcome[,2])
        
        ## Add Column names to DF.
        colnames(reportdata) <- c("heart attack","heart failure","pneumonia","Hospitals")
        
        
        if (dbug) suppressWarnings(print("DID NOT STOP 1"))  
        
        ## Sort by the outcome (oc), and hospital, row1 is your answer.
        answer <- reportdata[order(reportdata[oc],reportdata$Hospitals),]
        
        ## Load hospital name in answer1
        answer1 <- as.character(answer[1,"Hospitals"])
        
        ## Return hospital name in that state with lowest 30-day death rate
        suppressWarnings(print(answer1))
}

## This returns the 30 day death rate RANK of the hospital by outcome for a state.
rankhospital <- function(state,oc,num = "best") {
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that the State and outcome are valid
        
        ## Load valid num values
        if (num=="best") num=1
        else if (num=="worst") num=-1
                else num=as.numeric(num)
        
        
        ## Load valid state codes from complete file.
        valid_states <- unique(outcome[,7])
        
        ## Load valid hospitals from complete file.
        Hospitals <- outcome[,2]
        
        ## Hardcode the valid Outcomes
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        
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
        
        ## Disable Warning from conversions to numeric causing NAs.
        options(warn=-1)
        
        ##   print(c("not valid =",notvalid))
        
        if(is.na(num[1])) {
                ## dbug=TRUE
                ## if (dbug) suppressWarnings(print(num)) 
                stop("NA",call.=TRUE)
        }
        
        if (!notvalidST) {
                stop("invalid state",call.=TRUE)
        }
        
        if (!notvalidOUTCOME) {
                stop("invalid outcome",call.=TRUE)
        }
        
        ## Reduce outcome to contain rows limited to the State selected
        outcome <- outcome[outcome$State==state,]
        
        ## Convert the columns from STRING to numeric
        outcome[,11] <- as.numeric(outcome[,11])  ## HA
        outcome[,17] <- as.numeric(outcome[,17])  ## HF
        outcome[,23] <- as.numeric(outcome[,23])  ## PN
        
        ## Re-enable Warnings   
        options(warn=0)
        
        ## Get list of Hospitals for this State
        Hospitals <- outcome[,2]
        
        ## Build DF of HA,HF,PN,Hospitals for this State.
        reportdata <- data.frame(outcome[,11],outcome[,17],outcome[,23],outcome[,2])
        
        ## Add Column names to DF.
        colnames(reportdata) <- c("heart attack","heart failure","pneumonia","Hospitals")
        
        
        if (dbug) suppressWarnings(print("DID NOT STOP 1"))  
        
        ## Sort by the outcome (oc), and hospital, row1 is your answer.
        answer <- reportdata[order(reportdata[oc],reportdata$Hospitals),]
        ## Remove the NA cases
        good <- complete.cases(answer[]) ## Find all the completed rows
        answer <- answer[good,]
        
        worst <- NROW(answer) ## last one...is worst.
        
        ## Load hospital name in answer1
        
        if (num==1) answer1 <- as.character(answer[1,"Hospitals"])
                else if (num==-1) answer1 <- as.character(answer[worst,"Hospitals"])
                        else answer1 <- as.character(answer[num,"Hospitals"])
        
        ## Return hospital name in that state with given RANK 30-day death rate
        suppressWarnings(print(answer1))        
        
}