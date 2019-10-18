test <- function(outcome)
for(i in state.abb) {
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
  print(orderedData[num,1])
}

data2 <- subset(data, select = c(2, 7, 11, 17, 23))
data2[, c(3:5)] <- sapply(data2[, c(3:5)], as.numeric)
names(data2) <- c("hospital", "state", "heart attack", "heart failure", 
                  "pneumonia")
if(outcome == "heart attack") {
  attackData <- subset(data2, select = c(1,2,3))
  orderedData <- attackData[order(data2$`heart attack`,data2$hospital),]
  orderedData2 <- na.omit(orderedData)
}
if(outcome == "heart failure") {
  attackData <- subset(data2, select = c(1,2,4))
  orderedData <- attackData[order(data2$`heart failure`,data2$hospital),]
  orderedData2 <- na.omit(orderedData)
}
if(outcome == "pneumonia") {
  attackData <- subset(data2, select = c(1,2,5))
  orderedData <- attackData[order(data2$`pneumonia`,data2$hospital),]
  orderedData2 <- na.omit(orderedData)
}