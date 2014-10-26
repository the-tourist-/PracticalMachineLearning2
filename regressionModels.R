require(caret)

set.seed(123)
regressionModels <- list()
tc <- trainControl("cv", 10, savePredictions=T)
for(classe in levels(classification$classe))
  regressionModels[[classe]] <- train(trainingStd, as.numeric(classification[,classe]), method="glm", family=binomial, trControl=tc)

prediction <- list()
for(classe in levels(classification$classe))
  prediction[[classe]] <- predict(regressionModels[[classe]], trainingStd)

prediction$classe <- apply(as.data.frame(prediction), 1, function(x) names(x)[which(x==max(x))])

print(confusionMatrix(prediction$classe, classification$classe))

myTestPrediction <- list()
for(classe in levels(myTestClassification$classe))
  myTestPrediction[[classe]] <- predict(regressionModels[[classe]], myTestStd)

myTestPrediction$classe <- apply(as.data.frame(myTestPrediction), 1, function(x) names(x)[which(x==max(x))])

print(confusionMatrix(myTestPrediction$classe, myTestClassification$classe))