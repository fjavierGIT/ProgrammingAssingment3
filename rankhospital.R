rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) #outcomes names list
  
  ## Check that state and outcome are valid
  if(!state %in% myData$State) {
    stop("invalid state")
  }
  if(!outcome %in% names(outcomes)) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest outcome rate
  mynewData <- subset(myData, myData$State==state,na.rm=TRUE)  #Select by State
  mynewData[,outcomes[outcome]] <- as.numeric(mynewData[,outcomes[outcome]]) #Coerce outcome column to numeric
  mynewData <- na.omit(mynewData[order(mynewData[,outcomes[outcome]],mynewData[,2]),]) #Sort by Outcome&Name
  
  ## Determine num for 'best' and 'worst'
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(mynewData[, outcomes[outcome]])
    }
  }
  ## Rated value
  return(c(mynewData[num,2]))
}