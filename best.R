best <- function(state, outcome) {
        ## Read outcome data
        data1<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        states_all<-data1$State
        if (!state %in% states_all){
                stop("invalid state")
        }
        
        outcomes_list=c("heart attack","heart failure","pneumonia")
        if (!outcome %in% outcomes_list){
                stop("invalid outcome")
     s   }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        #x<-data1$Hospital.Name
        #mx<-tapply(data1[,11],x,max)
        outcome_col<-c(11,17,23)
        ind<-which(outcomes_list==outcome)
        ind_c<-outcome_col[ind]
        
        data1[,ind_c]<-as.numeric(data1[,ind_c])
        bad<-is.na(data1[,ind_c])
        data1<-data1[!bad,]
        
        ind<-which(data1$State == state, arr.ind = TRUE)
        data1<-data1[ind,]
        
        ind<-which(data1[,ind_c] == min(data1[,ind_c],na.rm=TRUE), arr.ind = TRUE)
        rs<-data1[ind,2]
        print(rs)
}