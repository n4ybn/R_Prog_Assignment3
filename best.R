## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
## rc <- setwd("C:/Downloads/Coursera/R_Prog_Assignment3")
rc <- setwd("C:/downloads/Coursera/R-Programming/R_Prog_Assignment3")
library(dplyr)
library(tidyr)
 best <- function (state,oc){
   
   ## Read outcome data
   
   outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
   
   ## Check that the State and outcome are valid
   
   valid_states <- unique(outcome[,7])
   
   Hospitals <- outcome[,2]
   
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
   
   
   outcome <- outcome[outcome$State==state,]
   outcome[,11] <- as.numeric(outcome[,11])  ## HA
   outcome[,17] <- as.numeric(outcome[,17])  ## HF
   outcome[,23] <- as.numeric(outcome[,23])  ## PN
   
   ## Re-enable Warnings   
   options(warn=0)
   
   Hospitals <- outcome[,2]
   

   reportdata <- data.frame(outcome[,11],outcome[,17],outcome[,23],outcome[,2])
   ## colnames(reportdata) <- c("HA","HF","PN","Hospitals")
   colnames(reportdata) <- c("heart attack","heart failure","pneumonia","Hospitals")
   
   
   ## suppressWarnings(print("DID NOT STOP"))  
   
   ## Sort by the outcome (oc), and hospital, row1 is your answer.
   answer <- reportdata[order(reportdata[oc],reportdata$Hospitals),]
   
   ## Return hospital name in that state with lowest 30-day death rate
  ## HA <- reportdata[order(reportdata$HA,reportdata$Hospitals),]
  ## HF <- reportdata[order(reportdata$HF,reportdata$Hospitals),]
  ## PN <- reportdata[order(reportdata$PN,reportdata$Hospitals),]
   
   ## suppressWarnings(print("DID NOT STOP"))
   
   ##Hospital <- data.frame(c(as.character(HA$Hospitals[1]),as.character(HF$Hospitals[1]),as.character(PN$Hospitals[1])))
   
   ##row.names(Hospital) <- c("heart attack","heart failure","pneumonia")
   
   answer1 <- as.character(answer[1,"Hospitals"])
   
  suppressWarnings(print(answer1))
 
   
 }