best <- function(state, outcome) {

  ## Read outcome data
  outcomeCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (is.na(match(state,unique(outcomeCM[,7])))) stop("invalid state")
  if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))) stop("invalid outcome")
 
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}