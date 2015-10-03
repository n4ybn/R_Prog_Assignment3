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

## Read outcome data
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character",skip=1,header=FALSE)


rankall <- function(oc, num = "best") {
        
       
        ## Column definitions of importance
        states.col              <- 7
        hospitals.col           <- 2
        HA.col                  <- 11
        HF.col                  <- 17
        PN.col                  <- 23
        states.contents         <- outcome[,states.col]
        hospitals.contents      <- outcome[,hospitals.col]
        HA.contents             <- as.numeric(outcome[,HA.col])
        HF.contents             <- as.numeric(outcome[,HF.col])
        PN.contents             <- as.numeric(outcome[,PN.col])

        ## Check that NUM and outcome are valid
        valid_states <- unique(outcome[,7])
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        
        notvalidOUTCOME=FALSE
        
        ## Load valid num values 
        if (num=="best") num=1 
        else if (num=="worst") num=-1 
        else num=as.numeric(num) 
        
        if (num==1) num=as.numeric(num)
        
        ## Disable Warning
        options(warn=-1)
        
        ## Check for valid Outcome
        for (j in valid_outcomes) {
                if (j == oc) {
                        notvalidOUTCOME = TRUE
                }
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
        
        results.colnames <- c("hospital","state")
      
        results <- data.frame(0,0)
        results <- setNames(results,results.colnames)
        num2 <- as.numeric(num)
        
        ## For each state, find the hospital of the given rank
        for (i in valid_states) {
                ##i <- "DC"
                ##num<-10
                if (oc=="heart attack")  {select.str1     <- paste0("select V2,V7,V11 from outcome where V11 IS NOT 'Not Available' and V7=='",i,"' order by V11,V2")}
                if (oc=="heart attack")  {select.str2     <- paste0("select V2,V7 from work where V7=='",i,"' order by V11,V2")}
                if (oc=="heart failure") {select.str1     <- paste0("select V2,V7,V17 from outcome where V17 IS NOT 'Not Available' and V7=='",i,"' order by V17,V2")}
                if (oc=="heart failure") {select.str2     <- paste0("select V2,V7 from work where V7=='",i,"' order by V17,V2")}
                if (oc=="pneumonia")     {select.str1     <- paste0("select V2,V7,V23 from outcome where V23 IS NOT 'Not Available' and V7=='",i,"' order by V23,V2")}               
                if (oc=="pneumonia")     {select.str2     <- paste0("select V2,V7 from work where V7=='",i,"' order by V23,V2")}
                
                ## Do the query
                HA.ranked <- sqldf(select.str1)
                work  <- HA.ranked
                work[,3]  <- as.numeric(work[,3])
                
                HA.ranked <- sqldf(select.str2)
                
                ## Set column names 
                HA.ranked  <- setNames(HA.ranked,results.colnames)
                
                
                if (num==-1) {
                       num2 <- as.numeric(nrow(HA.ranked))
                }
                
                if (nrow(HA.ranked)>=num2) {
                                 
                        results <- rbind(results,HA.ranked[num2,c("hospital","state")])
                } else {
                        results <- rbind(results,data.frame("hospital"="<NA>","state"=i))
                }
        }
        
        ## release the temp database
        sqldf()
        sqldf()

        ## Remove first row with 0's in it.
        results <- results[-1,]
      
        results <- sqldf("select * from results order by state")

}
