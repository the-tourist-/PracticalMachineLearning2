require(caret)

set.seed(123)

tc <- trainControl("cv", 10, savePredictions=T)

rfModel <- train(classification$classe ~ ., data=trainingStd, method="rf", trControl=tc)

prediction$rfclasse <- predict(rfModel, trainingStd)