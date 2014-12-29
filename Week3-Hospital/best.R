hospitalNameCol <- 2
stateCol <- 7

## EXERCICE 2 of assignment 3 for the R course of coursera
rank <- function(outcome, state = NULL, filterState = TRUE) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  problemCol <- switch(outcome,
                       "heart attack" = 11,
                       "heart failure" = 17,
                       "pneumonia" = 23)
  if(is.null(problemCol)){
    stop("invalid outcome")
  }
  
  if(filterState == TRUE){
    if(isStateValid(data, state) == FALSE){
      stop("invalid state")
    }

    data <- filterState(data, state)
  }
    
  data[, problemCol] <- as.numeric(data[, problemCol])
  data <- filterNA(data, problemCol)
  data <- data[order(data[, problemCol], data[, 2]), ]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data[, c(hospitalNameCol, stateCol, problemCol)]
}

best <- function(state, outcome) {
  as.character(rank(outcome, state)[1, 1])  
}

filterState <- function(data, state){
  data[data$State == state, ]
}

filterNA <- function(data, problemCol){
  data[!is.na(data[,problemCol]), ]
}
  
isStateValid <- function(outcome, state){
  state %in% outcome$State
}

test(best) <- function(){
  checkException(best("BB", "heart attack"))  
  checkException(best("TX", "heart attck"))  
  checkEquals("CYPRESS FAIRBANKS MEDICAL CENTER",  best("TX", "heart attack"))
  checkEquals("MAIMONIDES MEDICAL CENTER",  best("NY", "pneumonia"))
}

launchBest <- function(){
  clearLog()
  runTest(best)
  Log()
}

library(svUnit)