rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data2 <-
                read.csv(
                        "outcome-of-care-measures.csv",
                        colClasses = "character",
                        na.strings = "Not Available",
                        stringsAsFactors = FALSE
                )
        
        
        ## Check that state and outcome are valid

        outcomes_list <- c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% outcomes_list) {
                stop("invalid outcome")
        }
        
        
        ## For each state, find the hospital of the given rank

        outcome_col <- c(11, 17, 23)
        ind <- which(outcomes_list == outcome)
        ind_c <- outcome_col[ind]
        
        data2 <- data.frame(data2$Hospital.Name, data2$State, data2[, ind_c])
        names(data2) <- c("hospital", "state", "outcome")
        data2$outcome <- as.numeric(as.character(data2$outcome))
        bad <- is.na(data2$outcome)
        data2 <- data2[!bad, ]
        
        ii <-
                order(data2$state,
                      data2$outcome,
                      data2$hospital,
                      decreasing = FALSE)
        data2 <- data2[ii, ]
        
        df <- split(data2, data2$state)
        hosp_list<-sapply(df,function(x) as.character(x$hospital[num]))
        hosp_list<-sapply(df,function(x) as.character(x$hospital[num]))
        list_names<-names(hosp_list)
        
        if (num == "best") {
                num <- 1
                hosp_list<-sapply(df,function(x) as.character(x$hospital[num]))
        } else if (num == "worst") {
                hosp_list<-sapply(df,function(x) as.character(x$hospital[length(x$hospital)]))
        } else {
                hosp_list<-sapply(df,function(x) as.character(x$hospital[num]))
        }
        
       
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        states_list<-names(hosp_list)
        rs<-data.frame(hospital=hosp_list,state=states_list,row.names=states_list)

}