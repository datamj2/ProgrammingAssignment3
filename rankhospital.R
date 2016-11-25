rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data2 <-
                read.csv(
                        "outcome-of-care-measures.csv",
                        colClasses = "character",
                        na.strings = "Not Available",
                        stringsAsFactors = FALSE
                )
        
        ## Check that state and outcome are valid
        states_all <- data2$State
        if (!state %in% states_all) {
                stop("invalid state")
        }
        #
        outcomes_list <- c("heart attack", "heart failure", "pneumonia")
        if (!outcome %in% outcomes_list) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        
        
        #x<-data1$Hospital.Name
        #mx<-tapply(data1[,11],x,min)
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
        
        hosp_list <- df[[state]]
        hosp_list <- as.character(hosp_list$hospital)
        
        if (num == "best") {
                num <- 1
        }
        if (num == "worst") {
                num <- length(hosp_list)
        }
        
        rs <- hosp_list[num]
        print(rs)
}