rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  outcomeCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (is.na(match(state,unique(outcomeCM[,7])))) stop("invalid state")
  if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))) stop("invalid outcome")
  if (is.na(match(num,c("best","worst"))) & is.na(as.integer(num))) stop("invalid num")
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  options(warn = -1)
  Coloutcome <- switch (outcome, "heart attack" = 11,
                        "heart failure" = 17,
                        "pneumonia" = 23)
  SubsetOutcomeCM<-subset(outcomeCM,outcomeCM[,7]==state)
  SubsetOutcomeCM[,Coloutcome]<-sapply(SubsetOutcomeCM[,Coloutcome], as.character)
  SubsetOutcomeCM[,Coloutcome]<-sapply(SubsetOutcomeCM[,Coloutcome], as.numeric)
  SubsetOutcomeCM = na.omit(SubsetOutcomeCM)
  nHospital  <-  nrow(SubsetOutcomeCM)

  if (!is.na(as.numeric(num))) {
    if (num > nHospital) return(NA) else numRank <- num}
  else {
    numRank <- switch(num, best = 1, worst = nHospital)}
  
  HospitalOrder <- order(SubsetOutcomeCM[, Coloutcome], SubsetOutcomeCM[, 2])
  HospitalName<-SubsetOutcomeCM[HospitalOrder, ][numRank, 2]
}
