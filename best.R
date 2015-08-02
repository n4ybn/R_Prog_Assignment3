## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
library(dplyr)
library(tidyr)
 best <- function (state,oc){
   
   ## Read outcome data
   
   outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
   
   ## Check that the State and outcome are valid
   
   valid_states <- unique(outcome[,7])
   
   Hospitals <- outcome[,2]
   
   HA_counts <- outcome[,15]
   HA_counts <- as.numeric(HA_counts)
   
   HF_counts <- outcome[,21]
   HF_counts <- as.numeric(HF_counts)
   
   PN_counts <- outcome[,27]
   PN_counts <- as.numeric(PN_counts)
   
   reportdata <- data.frame(HA_counts,HF_counts,PN_counts,Hospitals)
   
   valid_outcomes <- c("heart attack","pneumonia","heart failure")

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
   
   print("DID NOT STOP")
   
   ## Return hospital name in that state with lowest 30-day death rate
   
   
 }