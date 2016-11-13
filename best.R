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
        
        data1[,11]<-as.numeric(data1[,11])
        bad<-is.na(data1[,11])
        data1<-data1[!bad,]
        
        max(data1[,11])
        ind<-which(data1[,11] == max(data1[,11]), arr.ind = TRUE)
        rs<-data1[ind,2]
        print(rs)
}