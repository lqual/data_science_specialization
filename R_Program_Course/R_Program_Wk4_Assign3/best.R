best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that state and outcome are valid
  if(is.na(match(state, c(state.abb, "DC")))) {
    stop("invalid state")
  }
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  state_data <- subset(data, data[, 7] == state)
  state_data2 <- subset(state_data, select = c(2, 7, 11, 17, 23))
  state_data2[, c(3:5)] <- sapply(state_data2[, c(3:5)], as.numeric)
  names(state_data2) <- c("hospital", "state", "heart attack", "heart failure", 
                          "pneumonia")
  if(outcome == "heart attack") {
    orderedData <- state_data2[order(state_data2$`heart attack`,state_data2$hospital),]
  }
  if(outcome == "heart failure") {
    orderedData <- state_data2[order(state_data2$`heart failure`,state_data2$hospital),]
  }
  if(outcome == "pneumonia") {
    orderedData <- state_data2[order(state_data2$`pneumonia`,state_data2$hospital),]
  }
  print(orderedData[1,1])
}