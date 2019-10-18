#question 1
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

#question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

training$Age <- cut2(training$Age, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$Age)

training$FlyAsh <- cut2(training$FlyAsh, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$FlyAsh)

training$Cement <- cut2(training$Cement, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$Cement)

training$BlastFurnaceSlag <- cut2(training$BlastFurnaceSlag, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$BlastFurnaceSlag)

training$Water <- cut2(training$Water, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$Water)

training$Superplasticizer <- cut2(training$Superplasticizer, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$Superplasticizer)

training$CoarseAggregate <- cut2(training$CoarseAggregate, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$CoarseAggregate)

training$FineAggregate <- cut2(training$FineAggregate, g = 5)
plot(training$CompressiveStrength, pch = 19, col=training$FineAggregate)

#question3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
par(mfrow = c(2,1), mar = c(2, 2, 2, 2))
hist(training$Superplasticizer)
hist(log10(training$Superplasticizer + 1))

#question4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
q4 <- preProcess(training[, 58:69], method = "pca", thresh = .8)

training[, 58:69]

#question5
library(caret)
library(AppliedPredictiveModeling)
RNGversion("3.0.0")
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
training <- training[, c(1, 58:69)]
testing <- testing[, c(1, 58:69)]
mdl1 <- train(diagnosis ~ ., data = training, method = "glm")
testmdl1 <- predict(mdl1, testing)
confusionMatrix(testing$diagnosis, testmdl1)

PCA <- preProcess(training[, 2:13], method = "pca", thresh = .8)
trainPCA <- predict(PCA, training)
testPCA <- predict(PCA, testing)
mdlPCA <- train(diagnosis ~ ., data = trainPCA, method = "glm")
testmdlPCA <- predict(mdlPCA, testPCA)
confusionMatrix(testPCA$diagnosis, testmdlPCA)