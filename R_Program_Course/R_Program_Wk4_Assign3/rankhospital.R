rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(is.na(match(state, c(state.abb, "DC")))) {
    stop("invalid state")
  }
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  state_data <- subset(data, data[, 7] == state)
  state_data2 <- subset(state_data, select = c(2, 7, 11, 17, 23))
  state_data2[, c(3:5)] <- sapply(state_data2[, c(3:5)], as.numeric)
  names(state_data2) <- c("hospital", "state", "heart attack", "heart failure", 
                          "pneumonia")
  if(outcome == "heart attack") {
    attackData <- subset(state_data2, select = c(1,2,3))
    orderedData <- attackData[order(state_data2$`heart attack`,state_data2$hospital),]
    orderedData2 <- na.omit(orderedData)
  }
  if(outcome == "heart failure") {
    attackData <- subset(state_data2, select = c(1,2,4))
    orderedData <- attackData[order(state_data2$`heart failure`,state_data2$hospital),]
    orderedData2 <- na.omit(orderedData)
  }
  if(outcome == "pneumonia") {
    attackData <- subset(state_data2, select = c(1,2,5))
    orderedData <- attackData[order(state_data2$`pneumonia`,state_data2$hospital),]
    orderedData2 <- na.omit(orderedData)
  }
  if(num == "best") {
    num <- 1
  }
  if(num == "worst") {
    num <- nrow(orderedData2)
  }
  if(num > nrow(orderedData2)) {
    num <- "NA"
  }
  print(orderedData[num,1])
}