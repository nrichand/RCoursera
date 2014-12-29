## EXERCICE 4 of assignment 3 for the R course of coursera

rankall <- function(outcome, num = "best") {
  #ranking <- rank(outcome, filterState = FALSE)
  #temp <- tapply(ranking[, 3], ranking[, 2], min)
  
  ## For each state, find the hospital of the given rank
  data <- read.csv("outcome-of-care-measures.csv")
  states <- levels(data$State)

  result <- matrix(nrow = 0, ncol = 2)
  for(state in states){
    hospital <- rankhospital(state, outcome, num)
    result <- rbind(result, c(hospital, state))
    print(state)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  finalRank <- data.frame(result)
  colnames(finalRank) <- c( 'hospital', 'state')
  finalRank
}

test(rankall) <- function(){ 

}

launchRankAll <- function(){
  clearLog()
  runTest(rankall)
  Log()
}

library(svUnit)