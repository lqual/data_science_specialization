#question1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
library(caret)
forestMdl <- train(y ~ ., data = vowel.train, method = "rf")
boostMdl <- train(y ~ ., data = vowel.train, method = "gbm")
predFor <- predict(forestMdl, vowel.test)
predBoost <- predict(boostMdl, vowel.test)
confusionMatrix(predFor, vowel.test$y)
confusionMatrix(predBoost, vowel.test$y)
#forest = .5909
#boost = .513
agree <- data.frame(actual = vowel.test$y, forest = predFor, boost = predBoost)
library(dplyr)
agree <- agree %>% mutate(agree = ifelse(forest == boost, forest, 0)) %>% 
        filter(agree != "0") %>% mutate(right = ifelse(agree == actual, 1, 0))
right <- agree %>% pull(right)
sum(right)/length(right)

#question2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
fitControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
fitrf <- train(diagnosis ~ ., data = training, method = "rf", trControl = fitControl)
fitgbm <- train(diagnosis ~ ., data = training, method = "gbm", trControl = fitControl)
fitlda <- train(diagnosis ~ ., data = training, method = "lda", trControl = fitControl)
predrf <- predict(fitrf, training)
predgbm <- predict(fitgbm, training)
predlda <- predict(fitlda, training)
predDF <- data.frame(predrf, predgbm, predlda, training$diagnosis)
combFit <- train(training.diagnosis ~ ., method = "rf", data = predDF)
stopCluster(cluster)
registerDoSEQ()
finalrf <- predict(fitrf, testing)
finalgbm <- predict(fitgbm, testing)
finallda <- predict(fitlda, testing)
finalDF <- data.frame(finalrf, finalgbm, finallda, testing$diagnosis)
names(finalDF) <- c("predrf", "predgbm", "predlda", "training.diagnosis")
finalcomb <- predict(combFit, finalDF)
confusionMatrix(finalcomb, testing$diagnosis)
#.8293
confusionMatrix(finalrf, testing$diagnosis)
#.7927
confusionMatrix(finalgbm, testing$diagnosis)
#.7927
confusionMatrix(finallda, testing$diagnosis)
#.7683

#question3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
fit <- train(CompressiveStrength ~ ., data = training, method = "lasso")
plot(fit$finalModel, xvar = "penalty", use.color = T)

#question4
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv",
              "gaData.csv")
library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
fit <- bats(tstrain)
fcast <- forecast(fit, h = (601-366), level = 95)
lo <- fcast$lower
hi <- fcast$upper
actual <- testing$visitsTumblr
df <- data.frame(lo, hi, actual)
names(df) <- c("lo", "hi", "actual")
library(dplyr)
df <- df %>% mutate(above = ifelse(actual > hi, 1, 0), 
                    below = ifelse(actual < lo, 1, 0)) %>%
        mutate(outside = above + below)
total <- df %>% pull(outside)
(length(total) - sum(total)) / length(total)

#question5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
fit <- svm(CompressiveStrength ~ ., data = training)
predict <- predict(fit, testing)
RMSE(testing$CompressiveStrength, predict)









