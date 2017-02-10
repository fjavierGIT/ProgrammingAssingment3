rankall <- function(outcome, num = "best") {
  ## Read outcome data
  myData <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23) #outcomes names list
  
  ## Check that outcome is valid
  if(!outcome %in% names(outcomes)) {
    stop("invalid outcome")
  }
  myData[,outcomes[outcome]] <- as.numeric(myData[,outcomes[outcome]]) #Coerce outcome column to numeric
  myData <<- na.omit(myData[order(myData[,outcomes[outcome]],myData[,2]),]) #Sort by Outcome&Name
  
  ## For each state, find the hospital of the given rank
  mynewData <<- split(myData,myData$State)
  
  hospitalNameFunction <- function()
  
  lapply(mynewData,nrow)
  lapply(split.data, hospitalNameFunction) 
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}