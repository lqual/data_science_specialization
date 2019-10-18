rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(is.na(match(outcome, c("heart attack", "heart failure", "pneumonia")))) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  result <- data.frame(matrix(nrow = 54, ncol = 2)) #build a data.frame
  colnames(result) <- c("hospital", "state") #name the columns
  for(i in 1:54) {
    state.abb2 <- sort(c(unique(data[, 7])))
    state <- state.abb2[i]
    state_data <- subset(data, data[, 7] == state)
    state_data2 <- subset(state_data, select = c(2, 7, 11, 17, 23))
    state_data2[, c(3:5)] <- sapply(state_data2[, c(3:5)], as.numeric)
    names(state_data2) <- c("hospital", "state", "heart attack", "heart failure", 
                            "pneumonia")
    if(outcome == "heart attack") {
      attackData <- subset(state_data2, select = c(1,2,4))
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
      rank <- 1
    } else if(num == "worst") {
      rank <- nrow(orderedData2)
    } else if(num > nrow(orderedData2)) {
      rank <- "NA"
    } else {
      rank <- num
    }
    result[i, 1] <- orderedData[rank,1]
    result[i, 2] <- state
    
  }
  result
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
}

