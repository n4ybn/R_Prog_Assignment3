## Best function section 2 of assignment
## accepts two arguments the 2-character State and Outcome Name
 best <- function (state,outcome){
   
   ## Read outcome data
   
   outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
   
   ## Check that the State and outcome are valid
   
   valid_states <- unique(outcome[,7])
   valid_outcomes <- c("heart attack","pneumonia","heart failure")

   notvalidST=FALSE
   notvalidOUTCOME=FALSE

   ## Check for valid states   
   for (i in valid_states) {
     if (i == state) {
       print(i)
       notvalidST=TRUE
       }
   }
  ## Check for valid Outcome
   for (j in valid_outcomes) {
     if (j==outcome) {
       notvalidOUTCOME = TRUE
     }
   }
   
   
   
   ##   print(c("not valid =",notvalid))
   
   if (!notvalidST) {
     stop("invalid state",call.=TRUE)
     return()
   }
   
   if (!notvalidOUTCOME) {
     stop("invalid outcome",call.=TRUE)
     return()
   }
   
   print("DID NOT STOP")
   
   ## Return hospital name in that state with lowest 30-day death rate
   
   
 }