rankhospital <- function(state, measure, num = "best") {
        ## Read outcome data
        outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid

        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        hospitalgroup<-split(outcome, outcome$State)
        statehospital<-hospitalgroup[[state]]
        if(measure=="heart attack"){m<-11}
        if(measure=="heart failure"){m<-17}
        if(measure=="pneumonia"){m<-23}
        
        statehospital[,m]<-as.numeric(statehospital[,m])## coercion
        com<-complete.cases(statehospital[,m])##remove NA values
        statehospital<-statehospital[com,] 
        
        if(num == "best"){num<-1}
        if(num == "worst"){num<-nrow(statehospital)}
        
        i<-order(statehospital[,m],statehospital[,2])
        rankone<-cbind(statehospital[,m],statehospital[,2])[i,]
        
        rankone[num,2]
        
}