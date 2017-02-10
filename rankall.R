rankall <- function(outcome, num = "best") {
    ## Read outcome data
  myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) #outcomes names list
  
  ## Check that outcome is valid
  if(!outcome %in% names(outcomes)) {
    stop("invalid outcome")
  }
  myData[,outcomes[outcome]] <- as.numeric(myData[,outcomes[outcome]]) #Coerce outcome column to numeric
  #myData <- na.omit(myData[order(myData[,7],myData[,outcomes[outcome]],myData[,2]),]) #Sort by State&Outcome&Name
  myData <- myData[order(myData[,7],myData[,outcomes[outcome]],myData[,2]),] #Sort by State&Outcome&Name
  
  ## For each state, find the hospital of the given rank
  mynewData <- split(myData,myData$State)
  
  hospitalNameFunction <- function(x) {
    num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(x), num))

    #print(c(nrow(x),x[num,2],x[num,7]))
    return(x[num,2])
    #print(head(nuevo))
  }
  
  #lapply(mynewData,nrow)
  mylist <<- unlist(lapply(mynewData, hospitalNameFunction))
  #print(mylist)
  nuevo <<- data.frame(hospital=mylist,state=names(mylist))
  #lapply(split.data, hospitalNameFunction) 
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}