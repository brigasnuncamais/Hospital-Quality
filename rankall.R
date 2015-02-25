rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  outcomeCM <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome and num are valid
  if (is.na(match(outcome,c("heart attack","heart failure","pneumonia")))) stop("invalid outcome")
  if (is.na(match(num,c("best","worst"))) & is.na(as.integer(num))) stop("invalid num")
  
  Coloutcome <- switch (outcome, "heart attack" = 11,
                        "heart failure" = 17,
                        "pneumonia" = 23)
  
  unique.states <- sort(unique(outcomeCM[,7]))
  options(warn = -1)
  
  RankedHospitalByState.df <- list()
  for(state in unique.states) {
    SubsetOutcomeCM <- subset(outcomeCM,outcomeCM[,7]==state)
    SubsetOutcomeCM[, Coloutcome] <- as.numeric(x=SubsetOutcomeCM[, Coloutcome])
    SubsetOutcomeCM = na.omit(SubsetOutcomeCM)
    if (!is.na(as.numeric(num))) {
      if(num < 1 || num > nrow(SubsetOutcomeCM)) {
        RankedHospitalByState.df <- rbind(RankedHospitalByState.df, list(NA, state))
        next
      }
      else numRank <- num
    }
    else numRank <- switch(num, best = 1, worst = nrow(SubsetOutcomeCM))
    SubsetOutcomeCM <- SubsetOutcomeCM[order(SubsetOutcomeCM[,Coloutcome], SubsetOutcomeCM$Hospital.Name), ]
    HospitalNames <- SubsetOutcomeCM[numRank, ]$Hospital.Name

    ## For each state, find the hospital of the given rank
    
    RankedHospitalByState.df <- as.data.frame(rbind(RankedHospitalByState.df, list(HospitalNames[1], state)))
  }

  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  colnames(x=RankedHospitalByState.df) <- c('hospital', 'state')
  RankedHospitalByState.df
}