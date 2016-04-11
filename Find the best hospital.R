best <- function(state,m) {
        ## Read outcome data
        outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        
        ## Return hospital name in that state with lowest 30-day death rate
        ## The hospitals in a specified state
        hospitalgroup<-split(outcome, outcome$State)
        statehospital<-hospitalgroup[[state]]
        
        ## find the hospital with the lowest measure
        if(m=="heart attack"){
                statehospital[,11]<-as.numeric(statehospital[,11])## coercion
                com<-complete.cases(statehospital[,11])##remove NA values
                statehospital<-statehospital[com,]
                
                ## find the lowest measure and the row number
                bestrow<-logical(nrow(statehospital))
                for(i in 1:nrow(statehospital)){
                        
                        if(statehospital[i,11] == min(statehospital[,11])){
                                bestrow[i]<-TRUE
                        }
                        else{
                                bestrow[i]<-FALSE
                        }
                        
                }
                ## return the hospital name w. the lowest measure
                bestone<-statehospital[bestrow,2]
                
        }
        if(m=="heart failure"){
                statehospital[,17]<-as.numeric(statehospital[,17])
                com<-complete.cases(statehospital[,17])
                statehospital<-statehospital[com,]
                bestrow<-logical(nrow(statehospital))
                for(i in 1:nrow(statehospital)){
                        if(statehospital[i,17] == min(statehospital[,17])){
                                bestrow[i]<-TRUE
                        }
                        else{
                                bestrow[i]<-FALSE
                        }
                        
                } 
                bestone<<-statehospital[bestrow,2]
        }
        if(m=="pneumonia"){
                statehospital[,23]<-as.numeric(statehospital[,23])
                com<-complete.cases(statehospital[,23])
                statehospital<-statehospital[com,]
                bestrow<-logical(nrow(statehospital))
                for(i in 1:nrow(statehospital)){
                        if(statehospital[i,23] == min(statehospital[,23])){
                                bestrow[i]<-TRUE
                        }
                        else{
                                bestrow[i]<-FALSE
                        }
                        
                } 
                bestone<<-statehospital[bestrow,2]
        }
        bestone
}