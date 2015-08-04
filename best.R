## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
rc <- setwd("C:/Downloads/Coursera/R_Prog_Assignment3")
library(dplyr)
library(tidyr)
 best <- function (state,oc){
   
   ## Read outcome data
   
   outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
   
   ## Check that the State and outcome are valid
   
   valid_states <- unique(outcome[,7])
   
   Hospitals <- outcome[,2]
   
   HA <- outcome[,15]
   suppressWarnings(HA <- as.numeric(HA))
   
   HF <- outcome[,21]
   suppressWarnings(HF <- as.numeric(HF))
   
   PN <- outcome[,27]
   suppressWarnings(PN <- as.numeric(PN))
   
   reportdata <- data.frame(HA,HF,PN,Hospitals)
   
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
   

   
   ## suppressWarnings(print("DID NOT STOP"))
   
   ## Return hospital name in that state with lowest 30-day death rate
   HA <- reportdata[order(reportdata$HA,reportdata$Hospitals),]
   HF <- reportdata[order(reportdata$HF,reportdata$Hospitals),]
   PN <- reportdata[order(reportdata$PN,reportdata$Hospitals),]
   
   Hospital <- data.frame(c(as.character(HA$Hospitals[1]),as.character(HF$Hospitals[1]),as.character(PN$Hospitals[1])))
   
   row.names(Hospital) <- c("heart attack","heart failure","pneumonia")
   
   answer <- as.character(Hospital[oc,])
   
  print(answer)
 
   
 }