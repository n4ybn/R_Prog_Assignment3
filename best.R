## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
## library(dplyr)
## library(tidyr)
rc <- setwd("C:/Downloads/Coursera/R_Prog_Assignment3")

best <- function (state,oc){
        
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Setup what is valid        
        valid_states <- unique(outcome[,7])
        valid_outcomes <- c("heart attack","pneumonia","heart failure")
        
        ## Get names of Hospitals
        Hospitals <- outcome[,2]
        States <- outcome[,7]
        
        ## Create new data frame of just completed rows of Hospitals and HA_counts
        HA_counts <- outcome[,15]
        suppressWarnings(HA_counts <- as.numeric(HA_counts))   
##        HA_counts_df <- data.frame(Hospitals,HA_counts)
        HA_counts_df <- data.frame(Hospitals,States,HA_counts)
        good <- complete.cases(HA_counts_df)
        HA_counts_df <- HA_counts_df[good,]
        ##   HA_counts_min <- HA_counts_df()
        
        ## Create new data frame of just completed rows of Hospitals and HF_counts
        
        HF_counts <- outcome[,21]
        suppressWarnings(HF_counts <- as.numeric(HF_counts))   
        HF_counts_df <- data.frame(Hospitals,States,HF_counts)
    ##    HF_counts_df <- data.frame(Hospitals,HF_counts)
        good <- complete.cases(HF_counts_df)
        HF_counts_df <- HF_counts_df[good,]
        
        
        ## Create new data frame of just completed rows of Hospitals and PN_Counts
        PN_counts <- outcome[,27]
        suppressWarnings(PN_counts <- as.numeric(PN_counts))
        PN_counts_df <- data.frame(Hospitals,States,PN_counts)
    ##  PN_counts_df <- data.frame(Hospitals,PN_counts)
        good <- complete.cases(PN_counts_df)
        PN_counts_df <- PN_counts_df[good,]
        
        
        ## reportdata <- data.frame(HA_counts,HF_counts,PN_counts,Hospitals,States)
        
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
        
        ## Begin the processing of the Data Frames for each valid type
        print("DID NOT STOP")
        
        ## Return hospital name in that state with lowest 30-day death rate
        
}