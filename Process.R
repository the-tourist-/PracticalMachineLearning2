require(moments)

includeColumns <- which(sapply(training, function(x) { mean(!is.na(x)) >= 0.99 & sd(x, na.rm=T) > 0 }, simplify=T))

scaleYJ <- preProcess(training[, includeColumns], method=c("center", "scale", "YeoJohnson"))

trainingStd <- predict(scaleYJ, training[, includeColumns])
myTestStd <- predict(scaleYJ, myTestSet[, includeColumns])
testingStd <- predict(scaleYJ, testing[, includeColumns])

kurt <- sapply(trainingStd, function(x) { kurtosis(x, na.rm=T)}, simplify=T)
skew <- sapply(trainingStd, function(x) { skewness(x, na.rm=T)}, simplify=T)
sd <- sapply(trainingStd, function(x) { sd(x, na.rm=T)}, simplify=T)
mean <- sapply(trainingStd, function(x) { mean(x, na.rm=T)}, simplify=T)

print(head(sort(mean, decreasing=T), n=20))
print(head(sort(sd, decreasing=T), n=20))
print(head(sort(kurt, decreasing=T), n=20))
print(head(sort(skew, decreasing=T), n=20))

for (i in 1:ncol(trainingStd)) {
  trainingStd[!is.na(trainingStd[, i]) & abs(trainingStd[, i])>3, i] <- 3 * sign(trainingStd[!is.na(trainingStd[, i]) & abs(trainingStd[, i])>3, i])
  myTestStd[!is.na(myTestStd[, i]) & abs(myTestStd[, i])>3, i] <- 3 * sign(myTestStd[!is.na(myTestStd[, i]) & abs(myTestStd[, i])>3, i])
  testingStd[!is.na(testingStd[, i]) & abs(testingStd[, i])>3, i] <- 3 * sign(testingStd[!is.na(testingStd[, i]) & abs(testingStd[, i])>3, i])
}

extractPCA <- preProcess(trainingStd, thresh=0.8, method="pca")

trainingPCA <- predict(extractPCA, trainingStd)
myTestPCA <- predict(extractPCA, myTestStd)
testingPCA <- predict(extractPCA, testingStd)

pairs(trainingPCA[,1:6], col=classification$classe)

