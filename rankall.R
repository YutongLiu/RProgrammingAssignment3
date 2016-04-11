rankall <- function(measure, num = "best") {
        ## Read outcome data
        outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        
        ## For each state, find the hospital of the given rank
        hospitalgroup<-split(outcome, outcome$State)
        if(measure=="heart attack"){m<-11}
        if(measure=="heart failure"){m<-17}
        if(measure=="pneumonia"){m<-23}
        sta<-names(hospitalgroup)
        hospital <- character(54L)
        state<- character(54L)
        
        for(i in 1:54){
        
                statehospital<-hospitalgroup[[sta[i]]]
                statehospital[,m]<-as.numeric(statehospital[,m])## coercion
                com<-complete.cases(statehospital[,m])##remove NA values
                statehospital<-statehospital[com,] 
        
                if(num == "best"){num<-1}
                if(num == "worst"){num<-nrow(statehospital)}
                if(nrow(statehospital)<num){
                        hospital[i]<-NA
                        next
                        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
                i<-order(statehospital[,m],statehospital[,2],statehospital[,7])
                rankone<-cbind(statehospital[,m],statehospital[,2],statehospital[,7])[i,]
                hospital[i]<-rankone[num,2]
                state[i]<-rankone[num,3]
                }
        
        htable<-data.frame(hospital,state)
        htable
}