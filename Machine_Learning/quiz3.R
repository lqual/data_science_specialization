#question1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
training <- subset(segmentationOriginal, Case == "Train")
testing <- subset(segmentationOriginal, Case == "Test")
set.seed(125)
model <- train(Class ~ ., method = "rpart", data = training)
library(rattle)
fancyRpartPlot(model$finalModel)
#a = PS, b = WS, c = PS, d = NA

#question3
library(pgmm)
data(olive)
olive = olive[,-1]
model <- train(Area ~ ., method = "rpart", data = olive)
predict <- predict(model, newdata = as.data.frame(t(colMeans(olive))))
predict

#question4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
trainSA$chd <- as.factor(trainSA$chd)
testSA$chd <- as.factor(testSA$chd)
set.seed(13234)
model <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
               data = trainSA, method = "glm", family = "binomial")
trainPredict <- as.numeric(predict(model, trainSA))-1
library(dplyr)
trainAnsw <- cbind(trainSA, trainPredict)
trainAnsw$chd <- as.numeric(trainAnsw$chd)-1
trainAnsw <- trainAnsw %>% select(chd, trainPredict) %>% 
        mutate(wrong = abs(chd - trainPredict)) %>% pull(wrong)
sum(trainAnsw)/length(trainAnsw)

testPredict <- as.numeric(predict(model, testSA))-1
testAnsw <- cbind(testSA, testPredict)
testAnsw$chd <- as.numeric(testAnsw$chd)-1
testAnsw <- testAnsw %>% select(chd, testPredict) %>% 
        mutate(wrong = abs(chd - testPredict)) %>% pull(wrong)
sum(testAnsw)/length(testAnsw)

#question5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
library(randomForest)
model <- randomForest(y ~ ., data = vowel.train)
imp <- varImp(model)
imp <- imp %>% mutate(variable = c(1:10)) %>% arrange(desc(Overall))
imp





