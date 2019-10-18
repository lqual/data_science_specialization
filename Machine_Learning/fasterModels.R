#https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
#insert model here
#need trcontrol = fitControl
fit <- train(x,y, method="rf",data=Sonar,trControl = fitControl)
#need this to stop parallel processing
stopCluster(cluster)
registerDoSEQ()
