## EXERCICE 3 of assignment 3 for the R course of coursera
rankhospital <- function(state, outcome, num = "best") {
  ranking <- as.character(rank(outcome, state)[, 1])  

  if(num == "worst"){
    return(ranking[length(ranking)])
  } else if(num == "best"){
    num <- 1
  }
  
  ranking[num]  
}

test(rankhospital) <- function(){ 
  checkEquals("DETAR HOSPITAL NAVARRO", rankhospital("TX", "heart failure", 4))
  checkEquals("HARFORD MEMORIAL HOSPITAL", rankhospital("MD", "heart attack", "worst"))
  checkTrue(is.na(rankhospital("MN", "heart attack", 5000)))
}

launchRankHospital <- function(){
  clearLog()
  runTest(rankhospital)
  Log()
}

library(svUnit)